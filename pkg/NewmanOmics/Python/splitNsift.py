from __future__ import print_function
from os.path import join, dirname, abspath
import xlrd
from xlwt import Workbook
import requests
import json
import pandas as pd
# from xlrd.sheet import ctype_text

norms = join(dirname(dirname(abspath(__file__))), 'splitNsift', 'LuAd_data.xls')
# Open the workbook
xl_workbook = xlrd.open_workbook(norms)
# List sheet names, and pull a sheet by name
#
sheet_names = xl_workbook.sheet_names()
print('Sheet Names', sheet_names)

norm_sheet = xl_workbook.sheet_by_name(sheet_names[0])
tumr_sheet = xl_workbook.sheet_by_name(sheet_names[1])
# Or grab the first sheet by index
#  (sheets are zero-indexed)
#
norm_sheet = xl_workbook.sheet_by_index(0)
print ('Sheet name: %s' % norm_sheet.name)
tumr_sheet = xl_workbook.sheet_by_index(1)
print ('Sheet name: %s' % tumr_sheet.name)
# Pull the first row by index
#  (rows/columns are also zero-indexed)
#
num_rows = norm_sheet.nrows  # 1st row
num_cols = norm_sheet.ncols
# Print 1st row values and types
#
# For writing to sheet excel
wb = Workbook()


print ("NUMBER OF Rows: ", num_rows)
print ("NUMBER OF Cols: ", num_cols)
# rowfix = 0
name = 1
for c in range(1, num_cols):
    rowfix = 0
    wb = Workbook()
    temp1 = wb.add_sheet("sheet1")
# for c in range(0, num_cols-1):
    for r in range(0, num_rows):
        if(norm_sheet.cell(r,c).value and norm_sheet.cell(r,c).value != 0):
            temp1.write(rowfix, 0, norm_sheet.cell(r, 0).value)
            # temp2.write(rowfix, 0, norm_sheet.cell(r, 0).value)
            temp1.write(rowfix, 1, norm_sheet.cell(r,c).value)
            # temp2.write(rowfix, 1, tumr_sheet.cell(r,c).value)
            rowfix = rowfix + 1
    wb.save("norm"+str(name)+".xls")
    c = c + 2
    name = name + 1


name = 1
for c in range(1, num_cols):
    rowfix = 0
    wb = Workbook()
    temp1 = wb.add_sheet("sheet1")
# for c in range(0, num_cols-1):
    for r in range(0, num_rows):
        if(norm_sheet.cell(r,c).value and norm_sheet.cell(r,c).value != 0):
            temp1.write(rowfix, 0, tumr_sheet.cell(r, 0).value)
            # temp2.write(rowfix, 0, norm_sheet.cell(r, 0).value)
            temp1.write(rowfix, 1, tumr_sheet.cell(r,c).value)
            # temp2.write(rowfix, 1, tumr_sheet.cell(r,c).value)
            rowfix = rowfix + 1
    wb.save("tumr"+str(name)+".xls")
    c = c + 2
    name = name + 1


for i in range(1,58):
    xls_file = pd.read_excel('norm'+str(i)+'.xls', sheetname="sheet1")
    xls_file.to_csv('norm'+str(i)+'.csv', index = False)

for i in range(1,58):
    xls_file = pd.read_excel('tumr'+str(i)+'.xls', sheetname="sheet1")
    xls_file.to_csv('tumr'+str(i)+'.csv', index = False)
