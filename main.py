import pygame
import time
import subprocess
import random

# Grid parameters
CELL_SIZE = 100

# Colors
WHITE = (255, 255, 255)
BLACK = (0, 0, 0)
RED = (255, 0, 0)
BLUE = (0, 0, 255)
GREEN = (0, 255, 0)

pygame.init()
screen = pygame.display.set_mode((1000, 1000))
pygame.display.set_caption("Roomba Simulation")
clock = pygame.time.Clock()

def loadPlan(filename):
    with open(filename, "r") as f:
        return [line.strip().upper() for line in f.readlines()]

def drawGrid(roombaPos, cleanedCells, dirtyCells, size):
    screen.fill(WHITE)
    for x in range(size):
        for y in range(size):
            rect = pygame.Rect(x * CELL_SIZE, y * CELL_SIZE, CELL_SIZE, CELL_SIZE)
            # If the cell is cleaned, color it green
            if (x, y) in cleanedCells:
                color = GREEN
            # If the cell is dirty and not yet cleaned, color it gray
            elif (x, y) in dirtyCells:
                color = RED
            else:
                color = WHITE
            pygame.draw.rect(screen, color, rect)
            pygame.draw.rect(screen, BLACK, rect, 1)
    
    # Draw the Roomba
    roomba = pygame.Rect(
        roombaPos[0] * CELL_SIZE, roombaPos[1] * CELL_SIZE, CELL_SIZE, CELL_SIZE
    )
    pygame.draw.ellipse(screen, BLUE, roomba)
    pygame.display.flip()
    
def simulatePlan(plan, dirtyCells, size):
    position = [0, 0] 
    cleanedCells = set()

    for step in plan:
        pygame.event.pump()
        if step.startswith("MOVE-"):
            direction = step.split("-")[1]
            if direction == "UP":
                position[1] = max(0, position[1] - 1)
            elif direction == "DOWN":
                position[1] = min(size - 1, position[1] + 1)
            elif direction == "LEFT":
                position[0] = max(0, position[0] - 1)
            elif direction == "RIGHT":
                position[0] = min(size - 1, position[0] + 1)
        elif step.startswith("CLEAN-"):
            cleanedCells.add(tuple(position))
        
        drawGrid(position, cleanedCells, dirtyCells, size)
        time.sleep(0.5)  # Pause to visualize each step

def generatePlan(size):
    scriptPath = "script.lisp"
    lispPath = "clisp/clisp.exe"
    numDirtySpots = random.randint(1, size)
    dirtySpots = set()
    while len(dirtySpots) < numDirtySpots:
        x = random.randint(0, size - 1)
        y = random.randint(0, size - 1)
        dirtySpots.add((x,y))
    
    with open(scriptPath, "w") as f:
        f.write('(load "gps.lisp")\n')
        f.write('(load "ops.lisp")\n')
        f.write(f'(defparameter *grid-size* {size})\n')
        f.write('(defparameter *ops* (generate-ops *grid-size*))\n')
        initialState = ["roomba-at-0-0"]
        for x, y in dirtySpots:
            initialState.append(f"dirty-at-{x}-{y}")
        initialState = ' '.join(initialState)

        f.write(f'(defparameter *initial-state* \'({initialState}))\n')

        goalState = [f"cleaned-at-{x}-{y}" for x,y in dirtySpots]
        goalState.append(f"roomba-at-0-0")
        goalState = ' '.join(goalState)       
        f.write(f'(defparameter *goal-state* \'({goalState}))\n')
        f.write('(defparameter *plan* (gps *initial-state* *goal-state* *ops*))\n')

    subprocess.run([lispPath, scriptPath],stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, text=True, check=True)
    return dirtySpots

def main():
    gridSize = int(input("Enter a grid size value (e.g., 6 for a 6x6 grid):"))
    dirtySpots = generatePlan(gridSize)

    plan = loadPlan("plan.txt")  # Load the plan file
    running = True

    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False

        simulatePlan(plan, dirtySpots, gridSize)
        running = False  # Stop after one simulation

    pygame.quit()

if __name__ == "__main__":
    main()