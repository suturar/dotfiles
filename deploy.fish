#!/usr/bin/fish
for row in (cat MANIFEST)
    echo "===== Installing package '$row' ====="
    stow -v --no-folding --restow $row 
end

