#!/bin/bash

# Run this script (once) in WSL such that WSL can have access to write to the NTFS file system (of Windows)

sudo umount /mnt/c
sudo mount -t drvfs C: /mnt/c -o metadata,uid=1001,gid=1001,umask=22,fmask=111

# https://superuser.com/questions/1275940/wsl-mounted-file-permissions
# https://devblogs.microsoft.com/commandline/chmod-chown-wsl-improvements/