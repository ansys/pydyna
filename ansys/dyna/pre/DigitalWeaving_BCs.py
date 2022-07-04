#!/usr/bin/env python
# coding: utf-8

# # Digital Weaving Project: Boundary Condition Generation Script

# - The focus of this notebook will be to loop over all the `*.xls` files in the given folder.
# - In this Notebook, we will implement a Mapping file logic for all the parameter of Card 1 in *DEFINE_CURVE_TITLE

# In[1]:


import os
import numpy as np
from time import time
from pandas import read_excel
from pathlib import Path
from sys import exit
import PySimpleGUI as sg
from natsort import os_sorted


# ## Get the folder location

# In[2]:


# Use tkinter to prompt a dialog box
# Comment: This has been working intermittently. It either freezes sometimes (cell runs indefinitely) and has a small python box persistently open.

# from tkinter import Tk
# from tkinter.filedialog import askdirectory

# path = askdirectory(title='Select Folder') # shows dialog box and return the path
# print(path)


# In[3]:


# Comment: This has also been very inconsistent. Makes the run to go into an infinite loop

# import promptlib
# prompter = promptlib.Files()
# dir = prompter.dir()
# print(dir)


# In[4]:


# Manually input a folder
# dir_path = Path(str(input("Enter the path to the input files folder: ")))
# dir_path = Path('D:\OneDrive - ANSYS, Inc\Documents\Files_Others\Seattle_Engineers\Peter_Coxeter\Weaving_SOW_Project\Weaving Machine Export Files')
# dir_path = Path('/Users/sandeepmedikonda/OneDrive - ANSYS, Inc/Documents/Files_Others/Seattle_Engineers/Peter_Coxeter/Weaving_SOW_Project/Weaving Machine Export Files')


# In[5]:


# Use PySimpleGUI to select a folder

sg.theme('DefaultNoMoreNagging')
# sg.theme('Dark Blue 3')  # please make your creations colorful

outpath = os.path.join(os.getcwd(), 'Weaving Machine Export Files')

layout = [  [sg.Text('Folder Path...')],
            [sg.Input(outpath), sg.FolderBrowse()], 
            [sg.OK(), sg.Cancel()]] 

window = sg.Window('Select a Folder...', layout, font=("Helvectica",12), enable_close_attempted_event=True)

while True:             # Event Loop
    event, values = window.read()
    if event == 'OK' or event == 'Cancel':
        break
    elif event == sg.WINDOW_CLOSE_ATTEMPTED_EVENT and sg.popup_yes_no('Do you really want to exit?') == 'Yes':
        break

window.close()


# In[6]:


values


# In[7]:


if (values[0] == None) or (values[0] == '') or (event == 'Cancel'):
    sg.popup('''================================================
   ERROR: CANNOT PROCEED! Wrong user-input provided. 
   INFO: Please provide a folder...   
================================================''',title='Error...',font=("Helvectica",18), line_width=100)
    exit()
    # raise Exception("CANNOT PROCEED: Please provide a folder...")
else:
    dir_path = Path(values[0])


# In[8]:


dir_path


# In[9]:


if not dir_path.is_dir():
    sg.popup('''==========================================================
   ERROR: Provided directory doesn't exist. Please check the folder path.
   INFO: Please select a folder containing the machine files (*.xls)...
==========================================================''',title='Error...',font=("Helvectica",18), line_width=100)
    exit()
    # raise Exception("Provided directory doesn't exist. Please check the folder path...")


# In[10]:


# exp_files = list(dir_path.glob('**/*.xls')) # Old way of doing it... Doesn't sort numerically or in human readable form
# Old approach - Doesn't work if the file name is just 4005.xls etc. for example
# exp_files = sorted(dir_path.glob('**/*.xls'), key=lambda path: int(path.stem.rsplit("_", 1)[1])) # found this code here: https://stackoverflow.com/questions/58312120/how-to-naturally-sort-pathlib-objects-in-python

exp_files = os_sorted(dir_path.glob('**/*.xls')) # This approach uses the natsort module

max_no_files = len(exp_files)


# In[11]:


if max_no_files == 0:
    sg.popup('''=======================================================
   ERROR: No '*.xls' files found in the selected directory.                          
   INFO: Please select a folder containing the machine files (*.xls)...   
=======================================================''',title='Error...',font=("Helvectica",18), line_width=100)
    exit()
    # raise Exception("Provided directory doesn't exist. Please check the folder path...")


# ## Use of PySimpleGui to take inputs

# In[12]:


# Approach 2: Values are Spin Elements

sg.theme('DefaultNoMoreNagging')
# sg.theme('Dark Blue 3')
# sg.theme('BlueMono')

map_file_path = os.path.join(os.getcwd(), 'UHU_Axis_Mapping_V2.xlsx')

layout = [  [sg.Text('Folder Path...')],
            [sg.Input(outpath), sg.FolderBrowse()], 
            [sg.OK(), sg.Cancel()]] 

window = sg.Window('Select a Folder...', layout, font=("Helvectica",12), enable_close_attempted_event=True)

width1 = 19
width2 = 10
width3 = 33
just_flag_1 = 'right'
just_flag_2 = 'center'
just_flag_3 = 'left'
toggle_disabled1 = True
toggle_disabled2 = True
toggle_disabled3 = True
toggle_disabled4 = True
toggle_disabled5 = True
toggle_disabled6 = True
toggle_disabled7 = True
toggle_disabled8 = True
dattyp_cuslist = [-100,-2,0,1,6]
layout = [
            [sg.Text('Select the Mapping File:', size=(width1,1), justification=just_flag_1), sg.InputText(map_file_path, size=(46,1), justification=just_flag_3, key='-MAPFILE1-'), sg.FileBrowse("Open File...", file_types = (('Excel Files', '*.xlsx'),),key='-MAPFILE2-')],
            # [sg.Text('Curve ID:', size=(width1,1), justification=just_flag_1), sg.Spin(values=[i for i in range(1, 100000)], initial_value=8001, size=(width2, 1), disabled=True, k='-CID-'), sg.Checkbox('Use File Names as Curve IDs instead', default=True, k='-CID1-')],
            [sg.Text('Curve ID:', size=(width1,1), justification=just_flag_1), sg.Spin(values=[i for i in range(1, 100000)], initial_value=8001, size=(width2, 1), disabled=toggle_disabled1, k='-CID-'), 
             sg.Radio('Use File Names as Curve IDs instead', 'RADIO1', size=(width3,1),default=True, k='-CID1-'), sg.Button('Toggle', k='-TOGGLE1-')],
            [sg.Text('SIDR :', size=(width1,1), justification=just_flag_1), sg.Spin(values=[i for i in range(0, 3)], initial_value=0, size=(width2, 1), disabled=toggle_disabled2, k='-SIDR-'),
             sg.Radio('Use SIDR values from the Mapping File', 'RADIO2', size=(width3,1),default=True, k='-SIDR1-'), sg.Button('Toggle', k='-TOGGLE2-')],
            [sg.Text('SFA :', size=(width1,1), justification=just_flag_1), sg.Spin(values=[i for i in np.arange(-100, 100, 0.5).tolist()], initial_value=1.0, size=(width2, 1), disabled=toggle_disabled3, k='-SFA-'),
             sg.Radio('Use SFA values from the Mapping File', 'RADIO3', size=(width3,1),default=True, k='-SFA1-'), sg.Button('Toggle', k='-TOGGLE3-')],
            [sg.Text('SFO :', size=(width1,1), justification=just_flag_1), sg.Spin(values=[i for i in np.arange(-100, 100, 0.5).tolist()], initial_value=1.0, size=(width2, 1), disabled=toggle_disabled4, k='-SFO-'),
             sg.Radio('Use SFO values from the Mapping File', 'RADIO4', size=(width3,1),default=True, k='-SFO1-'), sg.Button('Toggle', k='-TOGGLE4-')],
            [sg.Text('OFFA :', size=(width1,1), justification=just_flag_1), sg.Spin(values=[i for i in np.arange(-100, 100, 0.5).tolist()], initial_value=0.0, size=(width2, 1), disabled=toggle_disabled5, k='-OFFA-'),
             sg.Radio('Use OFFA values from the Mapping File', 'RADIO5', size=(width3,1),default=True, k='-OFFA1-'), sg.Button('Toggle', k='-TOGGLE5-')],
            [sg.Text('OFFO :', size=(width1,1), justification=just_flag_1), sg.Spin(values=[i for i in np.arange(-100, 100, 0.5).tolist()], initial_value=0.0, size=(width2, 1), disabled=toggle_disabled6, k='-OFFO-'),
             sg.Radio('Use OFFO values from the Mapping File', 'RADIO6', size=(width3,1),default=True, k='-OFFO1-'), sg.Button('Toggle', k='-TOGGLE6-')],
            [sg.Text('DATTYP :', size=(width1,1), justification=just_flag_1), sg.Spin(values=[i for i in dattyp_cuslist], initial_value=0, size=(width2, 1), disabled=toggle_disabled7, k='-DATTYP-'),
             sg.Radio('Use DATTYP values from the Mapping File', 'RADIO7', size=(width3,1),default=True, k='-DATTYP1-'), sg.Button('Toggle', k='-TOGGLE7-')],
            [sg.Text('LCINT :', size=(width1,1), justification=just_flag_1), sg.Spin(values=[i for i in range(0, 1000, 100)], initial_value=0, size=(width2, 1), disabled=toggle_disabled8, k='-LCINT-'),
             sg.Radio('Use LCINT values from the Mapping File', 'RADIO8', size=(width3,1),default=True, k='-LCINT1-'), sg.Button('Toggle', k='-TOGGLE8-')],
            [sg.HorizontalSeparator()],
            [sg.Stretch(), sg.Text('Enter Time Step:'), sg.Spin(values=[round(i,3) for i in np.arange(0.001, 100, 0.001).tolist()], initial_value=.001, size=(width2, 1), k='-TSTEP-'),sg.Stretch()],
            [sg.HorizontalSeparator()],
            [sg.Stretch(), sg.Submit(tooltip='Click to submit this form'), sg.Cancel(), sg.Stretch()]
         ]

window = sg.Window('*DEFINE_CURVE: Enter Card 1 Parameters...', layout, font=("Helvectica",12), resizable = True, enable_close_attempted_event=True)

while True:             # Event Loop
    event, values = window.read()
    if event == 'Cancel' or event == 'Submit':
        break
    elif event == "-MAPFILE2-":
        folder_or_file = sg.popup_get_file('Choose your Curve Title Mapping file (**.xlsx)')
    elif event == '-TOGGLE1-':
        toggle_disabled1 = not toggle_disabled1
        window['-CID-'].Update(disabled=toggle_disabled1)
        window['-CID1-'].Update(disabled=not toggle_disabled1)
        window['-CID1-'].Update(toggle_disabled1)
        window.Refresh()
    elif event == '-TOGGLE2-':
        toggle_disabled2 = not toggle_disabled2
        window['-SIDR-'].Update(disabled=toggle_disabled2)
        window['-SIDR1-'].Update(disabled=not toggle_disabled2)
        window['-SIDR1-'].Update(toggle_disabled2)
        window.Refresh()
    elif event == '-TOGGLE3-':
        toggle_disabled3 = not toggle_disabled3
        window['-SFA-'].Update(disabled=toggle_disabled3)
        window['-SFA1-'].Update(disabled=not toggle_disabled3)
        window['-SFA1-'].Update(toggle_disabled3)
        window.Refresh()
    elif event == '-TOGGLE4-':
        toggle_disabled4 = not toggle_disabled4
        window['-SFO-'].Update(disabled=toggle_disabled4)
        window['-SFO1-'].Update(disabled=not toggle_disabled4)
        window['-SFO1-'].Update(toggle_disabled4)
        window.Refresh()
    elif event == '-TOGGLE5-':
        toggle_disabled5 = not toggle_disabled5
        window['-OFFA-'].Update(disabled=toggle_disabled5)
        window['-OFFA1-'].Update(disabled=not toggle_disabled5)
        window['-OFFA1-'].Update(toggle_disabled5)
        window.Refresh()
    elif event == '-TOGGLE6-':
        toggle_disabled6 = not toggle_disabled6
        window['-OFFO-'].Update(disabled=toggle_disabled6)
        window['-OFFO1-'].Update(disabled=not toggle_disabled6)
        window['-OFFO1-'].Update(toggle_disabled6)
        window.Refresh()
    elif event == '-TOGGLE7-':
        toggle_disabled7 = not toggle_disabled7
        window['-DATTYP-'].Update(disabled=toggle_disabled7)
        window['-DATTYP1-'].Update(disabled=not toggle_disabled7)
        window['-DATTYP1-'].Update(toggle_disabled7)
        window.Refresh()
    elif event == '-TOGGLE8-':
        toggle_disabled8 = not toggle_disabled8
        window['-LCINT-'].Update(disabled=toggle_disabled8)
        window['-LCINT1-'].Update(disabled=not toggle_disabled8)
        window['-LCINT1-'].Update(toggle_disabled8)
        window.Refresh()
    elif event == sg.WINDOW_CLOSE_ATTEMPTED_EVENT and sg.popup_yes_no('Do you really want to exit?') == 'Yes':
        break

window.close()

fnames_cb_flag = values['-CID1-']
sidr_flag = values['-SIDR1-']
sfa_flag = values['-SFA1-']
sfo_flag = values['-SFO1-']
offa_flag = values['-OFFA1-']
offo_flag = values['-OFFO1-']
dattyp_flag = values['-DATTYP1-']
lcint_flag = values['-LCINT1-']


# In[13]:


values


# In[14]:


if (values['-MAPFILE1-'] == None) or (values['-MAPFILE1-'] == '') or (event == 'Cancel'):
    sg.popup('''================================================
   ERROR: CANNOT PROCEED! Wrong user-input provided. 
   INFO: Please provide an Excel File...   
================================================''',title='Error...',font=("Helvectica",18), line_width=100)
    exit()
    # raise Exception("CANNOT PROCEED: Please provide an Excel File...")
else:
    map_file_path = Path(values['-MAPFILE1-'])

if not map_file_path.exists():
    sg.popup('''==========================================================
   ERROR: Provided file doesn't exist. Please check the file path.
   INFO: Please select a file containing the mapping information (*.xlsx)...
==========================================================''',title='Error...',font=("Helvectica",18), line_width=100)
    exit()
    # raise Exception("Provided file doesn't exist. Please check the file path...")

# Read the excel file provided by the user which maps the curve id's/file names to the curve title
map_df = read_excel(map_file_path)
map_df.dropna(inplace=True)

# If no. of columns are less than 9, throw an error
if len(map_df.columns) != 9:
    sg.popup('''==========================================================
   ERROR: The no. of columns don't match the expected no. of columns (9).
   INFO: Please double check the information in the mapping file...
==========================================================''',title='Error...',font=("Helvectica",18), line_width=100)
    exit()
    # raise Exception("The no. of columns don't match the expected no. of columns. Please double check the information in the mapping file...")

# The first column (which represents the curve id's) is read back in as float values for some reason, we are changing it to integers first.
map_df_keys = map_df.iloc[:, 0].astype(int).astype(str)

# We are using the zip function to Convert Two Columns from the Pandas Dataframe to a Dictionary
# https://cmdlinetips.com/2021/04/convert-two-column-values-from-pandas-dataframe-to-a-dictionary/
map_dict_ctitles = dict(zip(map_df_keys, map_df.iloc[:, 1]))
map_dict_cids = dict(zip(map_df_keys, map_df.iloc[:, 0].astype(int)))
map_dict_sidr = dict(zip(map_df_keys, map_df.iloc[:, 2]))
map_dict_sfa = dict(zip(map_df_keys, map_df.iloc[:, 3]))
map_dict_sfo = dict(zip(map_df_keys, map_df.iloc[:, 4]))
map_dict_offa = dict(zip(map_df_keys, map_df.iloc[:, 5]))
map_dict_offo = dict(zip(map_df_keys, map_df.iloc[:, 6]))
map_dict_dattyp = dict(zip(map_df_keys, map_df.iloc[:, 7]))
map_dict_lcint = dict(zip(map_df_keys, map_df.iloc[:, 8]))


# In[15]:


#map_dict_cids


# ## User Inputs: All of them are arrays

# In[16]:


# Enter the title
# title = ["{}: {}".format(values[0], exp_files[i].stem) for i in range(max_no_files)]
# title = str(Path(file_loc[0]).stem)

# Enter the time step below
# time_step = float(input('Enter the desired time step value: ')) # Use if input is needed
# time_step = 0.001
time_step = [float(values['-TSTEP-']) for i in range(max_no_files)]

# Curve ID
# curve_id = str(Path(file_loc[0]).stem)
# curve_id = '4005'
# if not values['-CID1-']: # Old Approach
#     curve_ids = ["{}".format(int(values['-CID-'])+i) for i in range(max_no_files)]
# else:
#     curve_ids = ["{}".format(exp_files[i].stem) for i in range(max_no_files)]

curve_id = values['-CID-']
    
# Create a list of Include file Names
define_curves_fnames = ["{}.k".format(exp_files[i].stem) for i in range(max_no_files)]

# SIDR
# sidr = int(input('Enter the flag for dynamic relaxation: ')) # Use if input is needed
# sidr = 0
# sidr = [int(values['-SIDR-']) for i in range(max_no_files)]
sidr = int(values['-SIDR-'])

# SFA
# sfa = float(input('Enter the scale factor for abscissa values: ')) # Use if input is needed
# sfa = 1.0
# sfa = [float(values['-SFA-']) for i in range(max_no_files)]
sfa = float(values['-SFA-'])

# SFO
# sfo = float(input('Enter the scale factor for ordinate values: ')) # Use if input is needed
# sfo = 1.0
# sfo = [float(values['-SFO-']) for i in range(max_no_files)]
sfo = float(values['-SFO-'])

# OFFA
# offa = float(input('Offset for abscissa values:: ')) # Use if input is needed
# offa = 0.0
# offa = [float(values['-OFFA-']) for i in range(max_no_files)]
offa = float(values['-OFFA-'])

# OFFO
# offo = float(input('Offset for ordinate values:: ')) # Use if input is needed
# offo = 0.0
# offo = [float(values['-OFFO-']) for i in range(max_no_files)]
offo = float(values['-OFFO-'])

# DATTYP
# dattyp = int(input('Data type: ')) # Use if input is needed
# dattyp = 0
# dattyp = [int(values['-DATTYP-']) for i in range(max_no_files)]
dattyp = int(values['-DATTYP-'])

# LCINT
# lcint = int(input('Enter the no. of discretization points: ')) # Use if input is needed
# lcint = 0
# lcint = [int(values['-LCINT-']) for i in range(max_no_files)]
lcint = int(values['-LCINT-'])


# ## User inputs from GUI

# ## Helper functions for dyna keycard conversion

# In[17]:


# This function changes input into string with a defined length
def to_ix(input, x):
    if type(input) != type(1):
        if type(input) != type(' '):
            input = str(input)
        if len(input) < x+1:
            return ' ' * (x - len(input)) + input
        try:
            input = "{:.3E}".format(float(input))
            return ' ' * (x - len(input)) + input
        except Exception:
            return ' ' * (x-1) + '0' 
    else:
        input = str(input)
        if len(input) < x+1:
            return ' ' * (x - len(input)) + input

# As the name suggests, this function changes input into string with a length 10
def change_to_string(input):
    output = ""
    for value in input:
        temp = to_ix(value, 10)
        output += temp
    return output


# ## Below code is for Testing Purposes and outside the major for loop

# In[18]:


# card1_string = []
# curve_id_popup = False
# troublesome_files = []

# for cnt, file in enumerate(exp_files):
#     fileName = file.name
#     fileName_noExt = file.stem
    
#     card1_value = []

#     if values['-CID1-']:
#         if fileName_noExt in map_dict_cids.keys():
#             card1_value.append(map_dict_cids[fileName_noExt])
#         else:
#             card1_value.append(curve_id+cnt)
#             curve_id_popup = True
#             troublesome_files.append(fileName_noExt+'.k')
#     else:
#         card1_value.append(curve_id+cnt)
    
#     if values['-SIDR1-']:
#         if fileName_noExt in map_dict_sidr.keys():
#             card1_value.append(map_dict_sidr[fileName_noExt])
#         else:
#             card1_value.append(sidr)
#     else:
#         card1_value.append(sidr)
        
#     if values['-SFA1-']:
#         if fileName_noExt in map_dict_sfa.keys():
#             card1_value.append(map_dict_sfa[fileName_noExt])
#         else:
#              card1_value.append(sfa)   
#     else:
#         card1_value.append(sfa)

#     if values['-SFO1-']:
#         if fileName_noExt in map_dict_sfo.keys():
#             card1_value.append(map_dict_sfo[fileName_noExt])
#         else:
#              card1_value.append(sfo)   
#     else:
#         card1_value.append(sfo)

#     if values['-OFFA1-']:
#         if fileName_noExt in map_dict_offa.keys():
#             card1_value.append(map_dict_offa[fileName_noExt])
#         else:
#             card1_value.append(offa)  
#     else:
#         card1_value.append(offa)

#     if values['-OFFO1-']:
#         if fileName_noExt in map_dict_offo.keys():
#             card1_value.append(map_dict_offo[fileName_noExt])
#         else:
#             card1_value.append(offo)  
#     else:
#         card1_value.append(offo)
        
#     if values['-DATTYP1-']:
#         if fileName_noExt in map_dict_dattyp.keys():
#             card1_value.append(map_dict_dattyp[fileName_noExt])
#         else:
#             card1_value.append(dattyp)  
#     else:
#         card1_value.append(dattyp)

#     if values['-LCINT1-']:
#         if fileName_noExt in map_dict_lcint.keys():
#             card1_value.append(map_dict_lcint[fileName_noExt])
#         else:
#             card1_value.append(lcint)  
#     else:
#         card1_value.append(lcint)

#     card1_string.append(change_to_string(card1_value))
    

# # Print a warning if the either the curve id is not found in the Mapping File or if the file name is alpha-numeric
# if curve_id_popup:
#     warning_text = '''WARNING: 
# This message is being shown becase, either: 
#     - The flag is checked to use Filename as a CURVE_ID, 
#       but the Filename is not an integer, or
#     - The Curve ID was not found in the Mapping file\n
# Will use a random Curve ID >= 8001 for the files below and proceed...
# (check other parameters carefully as well)
# '''

#     def PopupDropDown(title, text, values1):
#         right_click_menu = ['', ['Select All', 'Cut', 'Copy', 'Paste']]
#         MLINE_KEY = '-RCLICKMENU-'
#         window1 = sg.Window(title,
#             [[sg.Stretch(),sg.Text(warning_text, size=(54, 8), justification='left', font=("Calibri", 16), relief=sg.RELIEF_RIDGE, k='-TEXT HEADING-', enable_events=True),sg.Stretch()],
#             [sg.Frame("Check these Input Deck's carefully:",[[sg.Multiline(default_text = "{}".format(values1), key=MLINE_KEY, size=(68,4), right_click_menu=right_click_menu, font='Courier 10')]])],
#             [sg.OK()]
#         ])

#         while True:             # Event Loop
#             event1, values1 = window1.read()
#             if event1 == 'OK':
#                 break
#             elif event == sg.WINDOW_CLOSE_ATTEMPTED_EVENT and sg.popup_yes_no('Do you really want to exit?') == 'Yes':
#                 break

#         window1.close()
#         #return None if event1 != 'OK' else values1['-DROP-']

#     # -----------------------  Calling your PopupDropDown function -----------------------

#     # values = ['choice {}'.format(x) for x in range(30)]
#     Warning_listToStr = ''.join([str(elem)+'\n' for elem in troublesome_files])
#     PopupDropDown('WARNING', warning_text, Warning_listToStr)


# In[19]:


# card1_string
# troublesome_files


# ## Iterate over the files in the selected folder

# In[20]:


starttime = time()
# sg.theme('DarkBlack')
sg.theme('DefaultNoMoreNagging')

card1_string = []
curve_id_popup = False
troublesome_files = []

message = 'Writing file: '
cnt = 0

for file in exp_files:
    fileName = file.name
    fileName_noExt = file.stem
    pmeter = sg.one_line_progress_meter('Progress bar...', cnt+1, len(exp_files), 'Writing file: ' + fileName_noExt + '.k', 
                                        orientation = 'h', size=(30, 40), grab_anywhere = True, keep_on_top = True)
    # print(not pmeter,cnt!=len(exp_files)-1)
    if not pmeter and cnt!=len(exp_files)-1:
        sg.popup('Generation of the input decks has been ABORTED')
        break

#     print(fileName)
#     print(fileName_noExt)
    
    # Read the excel file into a pandas dataframe and rename the column to 'Velocity'
    df = read_excel(file,header=None)
    df.rename(columns={0: "Velocity"}, inplace = True)
    
    max_len = len(df) # Obtaining the max. length for the time generation 
#     print(max_len)
    
    end_time = time_step[cnt] * max_len # Calculate the end time
#     print(end_time)
    
    # Create an array for time based on the input time-step
    time_arr = np.linspace(0,end_time, num=max_len, endpoint=False) 
        
    # Add the time array into the dataframe as a column
    df['time'] = time_arr 
    
    #------------------------------------------------------------------------------------------------------------------
    # Generating the Card 1 String from the dictionary

    card1_value = []
    card1_string = []

    if values['-CID1-']:
        if fileName_noExt in map_dict_cids.keys():
            card1_value.append(map_dict_cids[fileName_noExt])
        else:
            card1_value.append(curve_id+cnt)
            curve_id_popup = True
            troublesome_files.append(fileName_noExt+'.k')
    else:
        card1_value.append(curve_id+cnt)

    if values['-SIDR1-']:
        if fileName_noExt in map_dict_sidr.keys():
            card1_value.append(map_dict_sidr[fileName_noExt])
        else:
            card1_value.append(sidr)
    else:
        card1_value.append(sidr)
        
    if values['-SFA1-']:
        if fileName_noExt in map_dict_sfa.keys():
            card1_value.append(map_dict_sfa[fileName_noExt])
        else:
             card1_value.append(sfa)   
    else:
        card1_value.append(sfa)

    if values['-SFO1-']:
        if fileName_noExt in map_dict_sfo.keys():
            card1_value.append(map_dict_sfo[fileName_noExt])
        else:
             card1_value.append(sfo)   
    else:
        card1_value.append(sfo)

    if values['-OFFA1-']:
        if fileName_noExt in map_dict_offa.keys():
            card1_value.append(map_dict_offa[fileName_noExt])
        else:
            card1_value.append(offa)  
    else:
        card1_value.append(offa)

    if values['-OFFO1-']:
        if fileName_noExt in map_dict_offo.keys():
            card1_value.append(map_dict_offo[fileName_noExt])
        else:
            card1_value.append(offo)  
    else:
        card1_value.append(offo)
        
    if values['-DATTYP1-']:
        if fileName_noExt in map_dict_dattyp.keys():
            card1_value.append(map_dict_dattyp[fileName_noExt])
        else:
            card1_value.append(dattyp)  
    else:
        card1_value.append(dattyp)

    if values['-LCINT1-']:
        if fileName_noExt in map_dict_lcint.keys():
            card1_value.append(map_dict_lcint[fileName_noExt])
        else:
            card1_value.append(lcint)  
    else:
        card1_value.append(lcint)

    card1_string.append(change_to_string(card1_value))
    
    #------------------------------------------------------------------------------------------------------------------
    
    # Write the arrays into a list with the desired formatting
    write_str = [] 
    for i in np.arange(len(df)):
        write_str.append('{0:20.3f}{1:20.6f}\n'.format(round(df['time'][i],6),round(df['Velocity'][i],6)))
    
    # Generate the Curve Titles
    if fileName_noExt in map_dict_ctitles.keys():
        curve_title = "{}: {}".format(map_dict_ctitles[fileName_noExt], fileName_noExt) 
    else:
        curve_title = "{}: {}".format('No Title Found', fileName_noExt)
        
    
    header_str = '''*KEYWORD
$
$
*DEFINE_CURVE_TITLE
{0}
$:
$     LCID      SIDR       SFA       SFO      OFFA      OFFO    DATTYP     LCINT
{1}
'''.format(curve_title,card1_string[0])
    
    footer_str = '''$
$
*END'''
    
    if not os.path.isdir(os.path.join(dir_path,'input_decks')):
        os.mkdir(os.path.join(dir_path,'input_decks'))
        dyna_folder = os.path.join(dir_path,'input_decks')
    else:
        dyna_folder = os.path.join(dir_path,'input_decks')
    
    with open(os.path.join(dyna_folder, fileName_noExt + '.k'), 'w+') as f:
        f.writelines(header_str)
        f.writelines(write_str)
        f.writelines(footer_str)
        f.close()
    
    cnt = cnt + 1

endtime = time()


# ## Print a warning if the either the curve id is not found in the Mapping File or if the file name is alpha-numeric

# In[21]:


# Print a warning if the either the curve id is not found in the Mapping File or if the file name is alpha-numeric

sg.theme('DefaultNoMoreNagging')

if curve_id_popup:
    warning_text = '''WARNING: 
This message is being shown becase, either: 
    - The flag to use Filename as a CURVE_ID is checked, 
      however, the Filename is not an integer.
      OR
    - The Curve ID was not found in the Mapping file\n
Will use a random Curve ID >= 8001 for the files below and proceed...
(check other CARD 1 parameters carefully as well)
'''

    def PopupDropDown(title, text, values1):
        right_click_menu = ['', ['Select All', 'Cut', 'Copy', 'Paste']]
        MLINE_KEY = '-RCLICKMENU-'
        window1 = sg.Window(title,
            [[sg.Stretch(),sg.Text(warning_text, size=(54, 9), justification='left', font=("Calibri", 16), relief=sg.RELIEF_RIDGE, k='-TEXT HEADING-', enable_events=True),sg.Stretch()],
            [sg.Frame("Check these Input Deck's carefully:",[[sg.Multiline(default_text = "{}".format(values1), key=MLINE_KEY, size=(68,5), right_click_menu=right_click_menu, font='Courier 10')]])],
            [sg.Stretch(), sg.OK(), sg.Stretch()]
        ])

        while True:             # Event Loop
            event1, values1 = window1.read()
            if event1 == 'OK':
                break
            elif event == sg.WINDOW_CLOSE_ATTEMPTED_EVENT and sg.popup_yes_no('Do you really want to exit?') == 'Yes':
                break

        window1.close()
        #return None if event1 != 'OK' else values1['-DROP-']

    # -----------------------  Calling your PopupDropDown function -----------------------

    # values = ['choice {}'.format(x) for x in range(30)]
    file_inp = os.path.join(dyna_folder)
    Warning_listToStr = ''.join([str(elem)+'\n' for elem in troublesome_files])
    # Warning_listToStr = ''.join([os.path.join(dyna_folder, str(elem) + '\n') for elem in troublesome_files]) # Write Entire Path
    PopupDropDown('WARNING...', warning_text, Warning_listToStr)


# In[22]:


os.path.join(dyna_folder, fileName_noExt + '.k')


# In[23]:


troublesome_files


# In[24]:


dyna_folder


# ## Write out a Single Include File

# In[25]:


header_str = '''*KEYWORD
$
$
$ =============
$ INCLUDE cards
$ =============
$
'''
content = ''
for curve in define_curves_fnames:
    content += '*INCLUDE\n' + '{}\n$\n'.format(curve)

footer_str = '''$
*END'''

if not os.path.isdir(os.path.join(dir_path,'input_decks')):
    os.mkdir(os.path.join(dir_path,'input_decks'))
    dyna_folder = os.path.join(dir_path,'input_decks')
else:
    dyna_folder = os.path.join(dir_path,'input_decks')

with open(os.path.join(dyna_folder, 'Motion_Curves.k'), 'w+') as f:
    f.writelines(header_str)
    f.writelines(content)
    f.writelines(footer_str)
    f.close()


# In[26]:


# Comment: OLD Way of Doing things using a pop-up

# sg.theme('Material1')
# sg.popup("The program generated a total of {} input decks in {} seconds".format(cnt, str(round(endtime-starttime,2))),
#          '*' * 12 + "Input decks can be found at the below location" + '*' * 12,
#          "{}".format(dyna_folder),
#          '*' * 60,
#          title='Summary',font=("ArialBold",18), line_width=70)


# In[27]:


# string1 = "The program generated a total of "
# string2 = '\033[1m' + str(max_no_files) + '\033[0m'
# string3 = " input decks in "
# string4 = '\033[1m' + str(round(endtime-starttime,2)) + '\033[0m'
# string5 =  " seconds."

# str_tot = string1 + string2 + string3 + string4 + string5
# print(str_tot)


# In[28]:


# sg.theme('Dark Blue 3')
sg.theme('DefaultNoMoreNagging')

right_click_menu = ['', ['Select All', 'Cut', 'Copy', 'Paste']]
MLINE_KEY = '-RCLICKMENU-'

def do_clipboard_operation(event, window, element):
    if event == 'Select All':
        element.Widget.selection_clear()
        element.Widget.tag_add('sel', '1.0', 'end')
    elif event == 'Copy':
        try:
            text = element.Widget.selection_get()
            window.TKroot.clipboard_clear()
            window.TKroot.clipboard_append(text)
        except:
            print('Nothing selected')
    elif event == 'Paste':
        element.Widget.insert(sg.tk.INSERT, window.TKroot.clipboard_get())
    elif event == 'Cut':
        try:
            text = element.Widget.selection_get()
            window.TKroot.clipboard_clear()
            window.TKroot.clipboard_append(text)
            element.update('')
        except:
            print('Nothing selected')

layout = [[sg.Stretch(),sg.Text('Summary of the program', size=(38, 1), justification='center', font=("Helvetica", 16), relief=sg.RELIEF_RIDGE, k='-TEXT HEADING-', enable_events=True),sg.Stretch()],
          [sg.Frame('Information:',[[sg.Text("The program generated a total of {} input decks in {} seconds".format(cnt, str(round(endtime-starttime,2))), justification='center', font=("Helvetica", 14))]],)],
          [sg.Frame("Input Deck's Path:",[[sg.Multiline(default_text = "{}".format(dyna_folder), key=MLINE_KEY, size=(68,4), right_click_menu=right_click_menu, font='Courier 10')]])],
          [sg.Stretch(), sg.Button('Exit',tooltip='Click to close this window'), sg.Stretch()]
          ]


window = sg.Window('Summary...', layout, enable_close_attempted_event=True)

mline:sg.Multiline = window[MLINE_KEY]

while True:             # Event Loop
    event, values = window.read()
    if event == 'Exit':
        break
    elif event == sg.WINDOW_CLOSE_ATTEMPTED_EVENT and sg.popup_yes_no('Do you really want to exit?') == 'Yes':
        break
    elif event in right_click_menu[1]:
        do_clipboard_operation(event, window, mline)

window.close()

