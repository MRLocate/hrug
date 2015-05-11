######################## Honolulu R User Group ########################

######################## swirl ########################
## Swirl is a perfect place to start learning R. 
## It also offers data science courses for advanced users

## Install swirl & its dependance 
install.packages("swirl")

## Call swirl and keep learning !!
library("swirl")

## Temporary exit swirl enviroment and back to R console
play()

## Return to swirl after play()
nxt()

## Skip current questions and get the correct answer
skip()

## Return to the main menu of swirl  
main()

## exit from swirl
bye()

## install a swirl course
install_from_swirl("Data Analysis")

######################## swirlify ########################

## swirlify is for developer creating new courses on swirl
## install swirlify
install.packages("devtools")
devtools::install_github(c("swirldev/swirl", "swirldev/swirlify"))

## Call swirlify
library(swirlify)

## initiate a new course
new_lesson("newLesson","newCourse")

## edit the course
## open the YAML file inside the new directory you just create, then save it.
## follow the syntax of YAML file (remember indenting is very important) for example:
## - Class: text
##   Output: "Welcome to the Honolulu R-User Group!"

## publish or install a new course into your local R
install_course_directory("C:/.../newCourse/")

## More tips on how to create a course:
##      Official repository of swirlify     
##            https://github.com/swirldev/swirlify

##      install a swirl course for learning swirlify
##            install_from_swirl("Writing swirl Courses")

