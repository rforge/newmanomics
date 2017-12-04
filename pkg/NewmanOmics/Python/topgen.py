from __future__ import print_function
from os.path import join, dirname, abspath
import xlrd
import requests
import json
from mech import mechanizer


for i in range(1, 59):
    fname = join(dirname(dirname(abspath(__file__))), 'test_data', '/Users/Gershkowitz/Desktop/nuResults/sample'+ str(i) +'.xls')

    # Open the workbook
    xl_workbook = xlrd.open_workbook(fname)

    # List sheet names, and pull a sheet by name
    sheet_names = xl_workbook.sheet_names()
    print('Sheet Names', sheet_names)

    xl_sheet = xl_workbook.sheet_by_name(sheet_names[0])

    # Or grab the first sheet by index
    #  (sheets are zero-indexed)
    xl_sheet = xl_workbook.sheet_by_index(0)
    print ('Sheet name: %s' % xl_sheet.name)

    num_rows = xl_sheet.nrows  # Number of rows
    num_cols = xl_sheet.ncols   # Number of columns


    entrez_id = ""
    tempName = ""
    print ("NUMBER OF Rows: " , num_rows)
    print ("NUMBER OF Cols: " ,num_cols)
    # In this case, 1 is where the patient names start
    for c in range(1, num_cols):
        for r in range(0, num_rows):
            if(r == 0):
                # Add name of patient to index zero of list
                tempName = (str((xl_sheet.cell(r, c).value)))
            elif(xl_sheet.cell(r, c).value > 3.8):
                # Add entras Id if cell's value is greater than 3.8
                entrez_id = entrez_id + (str(int(xl_sheet.cell(r, 0).value))) + ", "
        print("Name: ",tempName)
        print("IDs" , entrez_id)
        # Call topgene parser
        mechanizer(tempName, entrez_id)
        entrez_id = ""
