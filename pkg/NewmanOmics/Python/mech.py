import mechanize
from bs4 import BeautifulSoup as BS
import requests

def mechanizer(title, entrez):

    # Parses topgene TOPPFUN
    br = mechanize.Browser()
    br.set_handle_robots(False)
    br.open('https://toppgene.cchmc.org/enrichment.jsp')
    br.select_form(nr=0) # Select first form

    # Set entry type to Entrez ID
    br.form['type'] = ['ENTREZ']
    br.form['training_set'] = entrez
    r = br.submit()

    data = r.read()
    soup = BS(data, "lxml")
    # "userid" is custom ID assigned to user by topgene
    userid = soup.input["value"]
    br.open('https://toppgene.cchmc.org/input_enrichment.jsp?userdata_id='+str(userid))

    br.select_form(nr=0)
    br.form['pvaluemethod'] = ['HYPER_PMF']

    # Set feature disease to True, everything else to false
    print "USERID: "+ userid
    options = br.find_control("category").items
    for i in range(0, len(options)):
        if options[i].name == "Disease":
            options[i].selected=True
            print "True: "+ options[i].name
        else:
            options[i].selected=False
            print "False: "+ options[i].name
    br.submit()

    # Get results
    url = 'https://toppgene.cchmc.org/download.jsp?userdata_id=' + str(userid)
    r = requests.get(url, allow_redirects=True)
    # Write results to text file
    open("D"+title, 'w').write(r.content)
