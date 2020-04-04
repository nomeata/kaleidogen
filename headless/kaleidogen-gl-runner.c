#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <EGL/egl.h>
#include <GL/gl.h>
#include <GLES3/gl3.h>
#include <gbm.h>
#include <png.h>

// settings

struct my_display {
    struct gbm_device *gbm;
    EGLDisplay egl;
};

struct my_config {
    struct my_display dpy;
    EGLConfig egl;
};

struct my_window {
    struct my_config config;
    struct gbm_surface *gbm;
    EGLSurface egl;
};

static struct my_display
get_display(void)
{
    struct my_display dpy;

    int fd = open("/dev/dri/renderD128", O_RDWR | FD_CLOEXEC);
    if (fd < 0) {
        abort();
    }

    dpy.gbm = gbm_create_device(fd);
    if (!dpy.gbm) {
        abort();
    }


#ifdef EGL_MESA_platform_gbm
    dpy.egl = eglGetPlatformDisplayEXT(EGL_PLATFORM_GBM_MESA, dpy.gbm, NULL);
#else
    dpy.egl = eglGetDisplay((EGLNativeDisplayType)dpy.gbm);
#endif

    if (dpy.egl == EGL_NO_DISPLAY) {
        abort();
    }

    EGLint major, minor;
    if (!eglInitialize(dpy.egl, &major, &minor)) {
        abort();
    }

    return dpy;
}

static struct my_config
get_config(struct my_display dpy)
{
    struct my_config config = {
        .dpy = dpy,
    };

    EGLint egl_config_attribs[] = {
        EGL_BUFFER_SIZE,        32,
        EGL_DEPTH_SIZE,         EGL_DONT_CARE,
        EGL_STENCIL_SIZE,       EGL_DONT_CARE,
        EGL_RENDERABLE_TYPE,    EGL_OPENGL_ES2_BIT,
        EGL_SURFACE_TYPE,       EGL_WINDOW_BIT,
        EGL_NONE,
    };

    EGLint num_configs;
    if (!eglGetConfigs(dpy.egl, NULL, 0, &num_configs)) {
        abort();
    }

    EGLConfig *configs = malloc(num_configs * sizeof(EGLConfig));
    if (!eglChooseConfig(dpy.egl, egl_config_attribs,
                         configs, num_configs, &num_configs)) {
        abort();
    }
    if (num_configs == 0) {
        abort();
    }

    // Find a config whose native visual ID is the desired GBM format.
    for (int i = 0; i < num_configs; ++i) {
        EGLint gbm_format;

        if (!eglGetConfigAttrib(dpy.egl, configs[i],
                                EGL_NATIVE_VISUAL_ID, &gbm_format)) {
            abort();
        }

        if (gbm_format == GBM_FORMAT_ARGB8888) {
            config.egl = configs[i];
            free(configs);
            return config;
        }
    }

    // Failed to find a config with matching GBM format.
    abort();
}

static struct my_window
get_window(struct my_config config, unsigned int width, unsigned int height)
{
    struct my_window window = {
        .config = config,
    };

    window.gbm = gbm_surface_create(config.dpy.gbm,
                                    width, height,
                                    GBM_FORMAT_ARGB8888,
                                    GBM_BO_USE_RENDERING);
    if (!window.gbm) {
        abort();
    }

#ifdef EGL_MESA_platform_gbm
    window.egl = eglCreatePlatformWindowSurfaceEXT(config.dpy.egl,
                                                   config.egl,
                                                   window.gbm,
                                                   NULL);
#else
    window.egl = eglCreateWindowSurface(config.dpy.egl,
                                        config.egl,
                                        (EGLNativeWindowType)window.gbm,
                                        NULL);
#endif

    if (window.egl == EGL_NO_SURFACE) {
        abort();
    }

    return window;
}

// https://stackoverflow.com/a/14324292/946226
static void screenshot_png(const char *filename, unsigned int width, unsigned int height) {
    size_t i, nvals;
    const size_t format_nchannels = 4;
    nvals = format_nchannels * width * height;
    png_byte *pixels = malloc(nvals * sizeof(GLubyte));
    png_byte *png_bytes = malloc(nvals * sizeof(png_byte));
    png_byte **png_rows = malloc(height * sizeof(png_byte*));
    glReadPixels(0, 0, width, height, GL_RGBA, GL_UNSIGNED_BYTE, pixels);
    for (i = 0; i < nvals; i++)
        png_bytes[i] = pixels[i];
    for (i = 0; i < height; i++)
        png_rows[height - i - 1] = &png_bytes[i * width * format_nchannels];
    png_structp png = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
    if (!png) abort();
    png_infop info = png_create_info_struct(png);
    if (!info) abort();
    if (setjmp(png_jmpbuf(png))) abort();
    png_init_io(png, stdout);
    png_set_IHDR(
        png,
        info,
        width,
        height,
        8,
        PNG_COLOR_TYPE_RGBA,
        PNG_INTERLACE_NONE,
        PNG_COMPRESSION_TYPE_DEFAULT,
        PNG_FILTER_TYPE_DEFAULT
    );
    png_write_info(png, info);
    png_write_image(png, png_rows);
    png_write_end(png, NULL);
    png_destroy_write_struct(&png, &info);
}

char *load_file(const char *fname, unsigned int *len) {
  unsigned int alloc = 65536;
  *len = 0;
  char *result;

  FILE *fp = fopen(fname,"rb");
  if (!fp) {
    fprintf(stderr,"Error: Unable to read file %s\n",fname);
    exit(1);
  }

  result = (char*)malloc(65536);
  if (!result) {
    fprintf(stderr,"Error: Unable to allocate 64K buffer!\n");
    exit(1);
  }

  for(;;) {
    unsigned int chunk = fread(result + *len, 1, 65536, fp);
    *len += chunk;

    if (chunk != 65536) break;
    alloc += 65536;
    result = (char*)realloc(result,alloc);
    if (!result) {
      fprintf(stderr,"Error: Unable to reallocate %uK buffer!\n",alloc>>10);
      exit(1);
    }
  }
  fclose(fp);
  return result;
}


int main(int argc, char *argv[])
{
  if (argc < 4) {
    fprintf(stderr, "Usage: %s width height shader.vert shader.frag\n\nPrints the kaleidogen to stdout as .png", argv[0]);
    exit(1);
  }

  const unsigned int width = atoi(argv[1]);
  const unsigned int height = atoi(argv[2]);
  const char *vert_file = argv[3];
  const char *frag_file = argv[4];

  unsigned int vert_len;
  const char *vert_source = load_file(vert_file, &vert_len);
  fprintf(stderr, "Foo: %s\n", vert_source);
  unsigned int frag_len;
  const char *frag_source = load_file(frag_file, &frag_len);
  fprintf(stderr, "Foo: %s\n", frag_source);

  struct my_display dpy = get_display();
  struct my_config config = get_config(dpy);
  struct my_window window = get_window(config, width, height);

  eglBindAPI(EGL_OPENGL_API);
  EGLContext eglCtx = eglCreateContext(dpy.egl, config.egl, EGL_NO_CONTEXT, NULL);

  eglMakeCurrent(dpy.egl, window.egl, window.egl, eglCtx);

  glClearColor(1.0, 1.0, 1.0, 0.0);

  GLuint vao;
  glGenVertexArrays(1, &vao);
  glBindVertexArray(vao);

  float vertices[] = {
     -1.0f,  -1.0f,
      1.0f,  -1.0f,
     -1.0f,   1.0f,
      1.0f,  -1.0f,
      1.0f,   1.0f,
     -1.0f,   1.0f
  };

  GLuint vbo;
  glGenBuffers(1, &vbo);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

  glEnable(GL_BLEND);
  glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);

  glViewport(0, 0, width, height);

  glClear(GL_COLOR_BUFFER_BIT);

  GLuint vertexShader;
  vertexShader = glCreateShader(GL_VERTEX_SHADER);
  glShaderSource(vertexShader, 1, &vert_source, &vert_len);
  glCompileShader(vertexShader);

  GLint isCompiled = 0;
  glGetShaderiv(vertexShader, GL_COMPILE_STATUS, &isCompiled);
  assert(isCompiled == GL_TRUE);

  GLuint fragmentShader;
  fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);
  glShaderSource(fragmentShader, 1, &frag_source, &frag_len);
  glCompileShader(fragmentShader);

  isCompiled = 0;
  glGetShaderiv(fragmentShader, GL_COMPILE_STATUS, &isCompiled);
  assert(isCompiled == GL_TRUE);

  GLuint shaderProgram;
  shaderProgram = glCreateProgram();
  glAttachShader(shaderProgram, vertexShader);
  glAttachShader(shaderProgram, fragmentShader);
  glLinkProgram(shaderProgram);

  GLint isLinked = 0;
  glGetProgramiv(shaderProgram, GL_LINK_STATUS, (int *)&isLinked);
  assert(isLinked == GL_TRUE);

  glUseProgram(shaderProgram);

  GLint loc = glGetAttribLocation(shaderProgram, "a_position");
  glVertexAttribPointer(loc, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(float), (void*)0);
  glEnableVertexAttribArray(loc);

  glUniform2f(glGetUniformLocation(shaderProgram, "u_windowSize"), width, height);

  glUniform1f(glGetUniformLocation(shaderProgram, "u_extraData[0]"), 0.0f);
  glUniform1f(glGetUniformLocation(shaderProgram, "u_extraData[1]"), 500.0f);
  glUniform1f(glGetUniformLocation(shaderProgram, "u_extraData[2]"), 500.0f);
  glUniform1f(glGetUniformLocation(shaderProgram, "u_extraData[3]"), 500.0f);
  glUniform1f(glGetUniformLocation(shaderProgram, "u_extraData[4]"), 1.0f);

  glBindVertexArray(vao);
  glDrawArrays(GL_TRIANGLES, 0, 6);
  glFinish();

  screenshot_png("test.png", width, height);

  // 6. Terminate EGL when finished
  eglTerminate(dpy.egl);
  return 0;
}
