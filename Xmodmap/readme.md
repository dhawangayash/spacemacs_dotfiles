# Run this command
cd ~/
cd -
dir=$(pwd)
cd ~/
ln -s .Xmodmap $dir/Xmodmap/xmodmap

# what does this xmodmap do?
This xmodmap modifies the capslock key to home button.
This modification is useful as escape key is the modal switcher in vim, whereas the modal switcher in xah-fly-keys is <home>. This makes things quite incompatible when using xah-fly-keys and vim together.
