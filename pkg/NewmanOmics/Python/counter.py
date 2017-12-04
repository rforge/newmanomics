from collections import Counter
import csv
import mechanize
from bs4 import BeautifulSoup as BS
import requests
import urllib

master = []

with open('individidualcount.csv','wb') as out:
    csv_out=csv.writer(out)
    csv_out.writerow(['name','count'])


for j in range(1, 59):

    title = "N" + str(j)
    print(title)
    symbolfile = open(title)

    symbolslist = symbolfile.read()

    num_lines = sum(1 for line in open(title))

    array = []
    j=0
    while j<num_lines:
        b = symbolslist.split('\n')[j]
        array.append(b)
        j+=1



    tempDrugList = []
    for i in range(0, len(array)):
        if "Drug" in array[i]:
            tempDrugList.append(array[i].split()[2])
    # Remove duplicates in list of drugs
    tempDrugList = set(tempDrugList)
    tempDrugList = list(tempDrugList)

    # number of drugs
    count = len(tempDrugList)
    print(len(tempDrugList))

    master.extend(tempDrugList)

    # titleCount is a list of patient and corresponding number of drugs
    titleCount = []
    titleCount.append([title, count])

    # writes to csv as Patient, #number of drugs
    fd = open('individidualcount.csv','a')
    for row in titleCount:
        fd.write(str(row) + "\n")



# Checks to see if cancer.gov request is working
# r = requests.post("https://www.cancer.gov/about-cancer/treatment/drugs")
# print(r.status_code, r.reason)
# print(r.content)

print(master)
# Converts to tuple with drug/number of occurences
c = Counter(master)
# Sorts into most common first
mc = c.most_common()
print mc

cancgov = []
html = urllib.urlopen('https://www.cancer.gov/about-cancer/treatment/drugs').read()
soup = BS(html, "lxml")
for d in soup.find_all("ul", {"class": "no-bullets no-description"}):
    for b in d.find_all('li'):
        cancgov.append(b.string)

# q is list of drugs found in https://www.cancer.gov/about-cancer/treatment/drugs
# checks to see if is substring of words in the list
q = [e for e in master if e in cancgov]
print(q)

# Converts to tuple with drug/number of occurences
canc1 = Counter(q)
# Sorts into most common first
canc2 = canc1.most_common()
print("Cancer Drugs:"+str(canc2))


with open('drugslistpre.csv','wb') as out:
    csv_out=csv.writer(out)
    csv_out.writerow([master])

with open('drugslistpost.csv','wb') as out:
    csv_out=csv.writer(out)
    csv_out.writerow([q])
