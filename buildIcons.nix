{ runCommand, imagemagick }:
{ src }:
runCommand "android-icons" {
  inherit src;
  buildCommand = ''
    mkdir "$out"

    launcherIconSize() {
      case "$1" in
        l) echo 36x36 ;;
        m) echo 48x48 ;;
        tv) echo 64x64 ;;
        h) echo 72x72 ;;
        xh) echo 96x96 ;;
        xxh) echo 144x144 ;;
        xxxh) echo 192x192 ;;
      esac
    }

    for x in l m tv h xh xxh xxxh ; do
      local dir="$out/drawable-''${x}dpi"
      mkdir "$dir"
      convert -resize "$(launcherIconSize "$x")" -flatten "$src" "$dir/ic_launcher.png"
      convert -resize "$(launcherIconSize "$x")" -flatten "$src" "$dir/ic_launcher_background.png"
      convert -resize "$(launcherIconSize "$x")" -flatten "$src" "$dir/ic_launcher_foreground.png"
    done

    mkdir $out/drawable-anydpi-v26
    cat > $out/drawable-anydpi-v26/ic_launcher.xml <<__END__
  <?xml version="1.0" encoding="utf-8"?>
  <adaptive-icon xmlns:android="http://schemas.android.com/apk/res/android">
      <background android:drawable="@drawable/ic_launcher_background" />
      <foreground android:drawable="@drawable/ic_launcher_foreground" />
  </adaptive-icon>
  __END__
  '';
  nativeBuildInputs = [
    imagemagick
  ];
} ""
