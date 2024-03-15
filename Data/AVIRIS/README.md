The AVIRIS-NG output CSVs, clipped to the whole lake, are too big to push to github.

Follow these instructions to get them in your project:
1. Make a new folder in Data/AVIRIS/. Name this folder "lake". (This name is included in the .gitignore file, so you won't accidentally try to push all these files next time you commit).
2. Download the AVIRIS lake CSVs from google drive: https://drive.google.com/drive/folders/1EzxTcvNj2wLnZjBqUljJ95wp7WypUvrH?usp=drive_link
(if this link doesn't work you can find the files at "Earth Observations (satellite and airborne)/AVIRIS-NG/Processed/Lake_Clipped/30m_downsampled")
3. Move those files into Data/AVIRIS/lake
4. Now you should be able to use these files in any scripts and analyses within this project, without having to push or pull them.

**NOTE**: if these files are ever updated you will need to re-download the new version and replace the copy on your computer. This version is current as of July 6, 2023.
