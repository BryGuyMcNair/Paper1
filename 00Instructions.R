

#############################################
# Instructions for GitHub, OSG, etc.
# Bryan McNair, MS
# 10-28-2024
#############################################


To Start, open PowerShell and log on:
  PS C:\Users\brygu\OneDrive - The University of Colorado Denver\7 
    Dissertation\Paper 1 - QPMm-B Distributions> ssh bryan.mcnair@ap40.uw.osg-htc.org
Use Usual PW

### First Example
#Open/Create(?) the/a(?) Container:
  apptainer shell \
/cvmfs/singularity.opensciencegrid.org/opensciencegrid/osgvo-r:3.5.0
#Start R:
  Singularity :~/tutorial-R>  R
#Quit R:
  > q()
Save workspace image? [y/n/c]: n
Singularity :~/tutorial-R>
#Exit the container:
  Singularity :~/tutorial-R>  exit


### Can find additional tutorials here: https://github.com/orgs/OSGConnect/repositories?type=all
Workflow Tutorials¶
OSPool workflow tutorials on Github¶
All of the OSG provided tutorials are available as repositories on Github. These tutorials are tested regularly and should work as is, but if you experience any issues please contact us.
Install and setup a tutorial¶
On an OSPool Access Point, type the following to download a tutorial's materials:

$ git clone https://github.com/OSGConnect/<tutorial-name>
This command will clone the tutorial repository to your current working directory. cd to the repository directory and follow the steps described in the readme.md file. Alternatively, you can view the readme.md file at the tutorial's corresponding GitHub page.


# To get my files onto the access point server, I don't need FTP or anything like that. I need my files 
# to be in a repo on GitHub. Then, I can clone the repo onto the access point using something like:
# git clone 


#### To push files:
  # go to Git window
  # select files to stage
  # hit commit
  # 


### GitHub Token
  # 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




PS C:\Users\brygu> cd 'C:\Users\brygu\OneDrive - The University of Colorado Denver\7 Dissertation\Paper1'
echo "# Paper1" >> README.md
git init
git add README.md
git commit -m "first commit"
git branch -M main
git remote add origin https://github.com/BryGuyMcNair/Paper1
git push -u origin main
