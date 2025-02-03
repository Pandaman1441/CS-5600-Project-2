## Roomba Logic Guide

This is a guide on how to get the program to run. I assume a majority of people looking at this have used Python previously and have most of the libraries installed already but the one that might pop an error is pygame, so I recommend making sure that it is installed beforehand.

Users only need to interact with the main.py file to use this program everything else is handled automatically. 

Using a terminal, ensure we are inside the CS-5600-PROJECT-2 directory. From here we can simply use the command python main.py. This cause a new window named roomba simulation to pop up. Don't worry if you don't see much and return to the terminal, there a new prompt should show up asking for a grid size value. This program will create a square grid based on the value you input, so entering 6 will have the program create a 6 by 6 grid and so forth. Please enter numerical values as the program is not well equiped to handle incorrect or bad values. 

After entering your desired grid size the program should handle the rest and start the simulation and you can return to the roomba simulation window to watch it in action.

This program works by having the python file write a variable lisp script to the GPS(General Problem Solver). The GPS handles the logic and decides the steps and movements the roomba will take to complete its goal. The GPS will generate a plan in a file containing all the actions necessary to complete its goal. The python file will then read this plan and perform all the actions in order and display each action using pygame.