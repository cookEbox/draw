# Draw
## Introduction
Draw is a first attempt to produce a PPT clone aimed to fit a Mathematics Teachers
work flow.

## Phase 0.1.1
1. 
    - [ ] Selecable random question
        - [x] Random generated question 
        - [ ] A choice of question topics
        - [ ] A choice of question levels

2.
    - [ ] Multiple Questions displayed

3. 
    - [ ] Turn questions and answers into drawing areas

4. 
    - [ ] Save a page 

5. 
    - [ ] load a page

## Phase 0.1.0

## Tasks
1.  
    - [x] A Drawable interface 
2.  
    - [x] Multiple Pages 
3.  
    - [x] Resizable window 
4.  
    - [x] Add Pages Button
5.  
    - [x] Remove Pages Button
        - [x] A button that removes the current page 
        - [x] Adjust IORef to have new count
        - [x] Rename all labels 
6.  
    - [x] Change page labels to horizontal tabs
7.  
    - [x] Rubber & Pen Colour
        - [x] right click menu 
            - [x] menu appear
            - [x] draw button 
            - [x] rubber button 
            - [x] set colour function
8.      
    - [x] Insert Page
9.  
    - [x] Change from vbox to hbox for buttons
10. 
    - [x] Pop up window for questions
        - [x] make a notepad appear as a floating window with two tabs
        - [x] make the floating notepad closeable
11. 
    - [x] Refactor
        - [x] serperate into modules
        - [x] move all IORef to a single type 
            - This was not possible as surface needed to be seperate otherwise program crashed
            - Reduced to two IORef's marking complete
12. 
    - [x] Question Generator
        - [x] print to a tab in the floating window
        - [x] print questions to the first tab 
        - [x] print answers to the second tab

## Future Tasks 
- Save page 
- Save all pages
- Resizable drawing area for monitor size 
- Changing size of rubber and drawing Pen
- Clear annotations
- Move to new page when adding page
- Duplicate page
