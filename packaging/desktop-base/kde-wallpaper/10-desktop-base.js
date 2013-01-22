// Placed in /usr/share/kde4/apps/plasma-desktop/init/
// This script is run for new users, which do not have a .kde directory
// and it set's the default wallpaper for all activities

a = activities()

for (i in a) {
    a[i].wallpaperPlugin = 'image'
    a[i].wallpaperMode = 'SingleImage'
    a[i].currentConfigGroup = Array('Wallpaper', 'image')
    a[i].writeConfig('wallpaper', 'joy')                //change this in wheezy+1
    a[i].writeConfig('wallpaperposition', '2')          //enables croping
}
