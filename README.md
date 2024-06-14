# Draw
## Introduction
Draw is a first attempt to produce a PPT clone aimed to fit a Mathematics Teachers
work flow.

## Future Tasks 
- Save page 
- Save all pages
- Resizable drawing area for monitor size 
- Changing (as a user setting) size of rubber and drawing Pen
- Having a point for a cursor relative to draw size
- Clear annotations
- Move to new page when adding page
- Duplicate page

## [ ] Phase 0.1.1
### Tasks
1. 
    - [x] Selecable random question
        - [x] Random generated question 
        - [x] A choice of question topics
            - [x] Subtopics
        - [x] A choice of question levels

2. 
    - [x] Turn questions and answers into drawing areas
        - [x] Seperate out addPages into a basic pages function without buttons
        - [x] seperate out rightClickMenu into a basic menu without questions
        - [x] reuse basic functions (above) to make questions into an interactive drawing area
        - [x] render questions using Con.renderWithContext to render questions to drawing area

3.
    - [ ] Multiple Questions displayed

4. 
    - [ ] Save a page 

5. 
    - [ ] Load a page

## [X] Phase 0.1.0

### Tasks
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
