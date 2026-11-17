"""Convert the M2ara logo into a Windows .ico file for the installer.

Usage: python make-ico.py <source.png> <target.ico>
"""

import sys

from PIL import Image

SIZES = [(16, 16), (24, 24), (32, 32), (48, 48), (64, 64), (128, 128), (256, 256)]


def main() -> int:
    if len(sys.argv) != 3:
        print("usage: make-ico.py <source.png> <target.ico>", file=sys.stderr)
        return 2

    source, target = sys.argv[1], sys.argv[2]
    image = Image.open(source).convert("RGBA")

    # pad to a square canvas so the icon is not distorted
    side = max(image.size)
    square = Image.new("RGBA", (side, side), (0, 0, 0, 0))
    square.paste(image, ((side - image.width) // 2, (side - image.height) // 2), image)

    square.save(target, format="ICO", sizes=SIZES)
    print(f"wrote {target} from {source} ({side}x{side})")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
