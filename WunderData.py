import urllib2
#from bs4 import BeautifulSoup

##Create/open a file called wunder.txt (which will be a comma-delimited file), in write mode
#weatherdata = open('wunder-data.txt', 'w')
##Iterate through year, month, and day
#for y in range(1948, 2013):
#    for m in range(5, 11):
#        for d in range(1, 32):
#            #Open wunderground.com url
#            url = "http://www.wunderground.com/history/airport/KBLU/"+str(y)+ "/" +str(m)+"/"+str(d)+"/DailyHistory.html?format=1"
#            page = urllib2.urlopen(url)
#            dailyData = page.read()
#            weatherdata.write(dailyData, 'a')
#weatherdata.close()                            

#Create/open a file called wunder.txt (which will be a comma-delimited file), in write mode
def weather(year):
#   works for 1948
    fname = '../Climate/WeatherStationData/wunder-data-%d.txt' % year
    f = open(fname, 'w')
    #Iterate through month, and day
    for m in range(5, 11):
        for d in range(1, 32):
            #Open wunderground.com url
            url = "http://www.wunderground.com/history/airport/KBLU/"+str(year)+ "/" +str(m)+"/"+str(d)+"/DailyHistory.html?format=1"
            page = urllib2.urlopen(url)
            dailyData = page.read()
            f.write(dailyData)
    f.close()                            

#for year in range(1948, 1950):
#    weather(year)

def weather2(year):
#   works for 1949+
    #Create/open a file called wunder.txt (which will be a comma-delimited file), in write mode
    fname = '../Climate/WeatherStationData/wunder-data-%d.txt' % year
    f = open(fname, 'w')
    #Iterate through month, and day
    for m in range(5, 11):
        for d in range(1, 32):
            #Open wunderground.com url
            url = "http://www.wunderground.com/history/airport/KBLU/" + str(year) + "/" + str(m) + "/" + str(d) + "/DailyHistory.html?req_city=NA&req_state=NA&req_statename=NA&format=1"
            page = urllib2.urlopen(url)
            dailyData = page.read()
            f.write(dailyData)
    f.close() 

import glob
def wunderclean():
    ''' This function takes the output from the above functions and cleans it up so that 
        it will only have the weather data from the hours that I will want it from.
        Range of times desired is 10:00 AM through 6:59 PM
        The name of the first column (related to time of day) is inconsistent throughout
        the files. We have seen TimePDT, TimePST, and Time. We have decided not to convert TimePDT
        and PST to the same time. 'Time' seems to only appear once on a day in which the station
        malfunctioned and no data was recorded (June 6 1979). Another error arose from June 1998 
        in which the string printed "No daily or hourly history data available" which we have
        corrected by adding 'No daily' to the list of cues to skip the line for data collection.
    '''
    outname = '../Climate/WeatherStationData/wunderclean.txt'
    #outname = '../Climate/WeatherStationData/'+ raw_input('new file name')
    out = open(outname, 'w')
    for fname in glob.glob('../Climate/WeatherStationData/wunder-data-*.txt'):
        print 'opening %s' %fname.split('/')[-1]
        f = open(fname, 'r')
        flines = f.readlines()
        for line in flines:
            if line.startswith('Time') or len(line) < 2 or line.startswith('No daily'):
                #print 'header or blank'
                continue
            elif not indaterange(line):
                #print 'out of range'
                continue
            hour = int(line.split(':')[0])
            if 10 <= hour < 12 and 'AM' in line:
                out.write(line)
            elif 12 == hour and 'PM' in line:
                # Special case Noon
                out.write(line)
            elif 1 <= hour <= 6 and 'PM' in line:
                out.write(line)
        f.close()
    out.close() 

def indaterange(line):
    """ Checks whether the line is from a date during the fire season.
        Currently only checks that the date is in May before May 15th (May 15th is OK)
    """
    baddates = [ '05-%02i' % i for i in range(15) ]
    for date in baddates:
        if date in line:
            return False
    return True

import glob
def mesonetclean(newfile, filepath):
    ''' This function takes the output from the above functions and cleans it up so that 
        it will only have the weather data from the hours that I will want it from.
        Range of times desired is 10:00 AM through 6:59 PM
    '''
    outname = '../Climate/WeatherStationData/' + newfile
    #outname = '../Climate/WeatherStationData/'+ raw_input('new file name')
    out = open(outname, 'w')
    for fname in glob.glob('../Climate/WeatherStationData/'+ filepath):
        print 'opening %s' %fname.split('/')[-1]
        f = open(fname, 'r')
        flines = f.readlines()
        for line in flines:
            if line[0].isdigit():
                hour = int(line.split(',')[3])
                if 10 <= hour <= 18:
                    out.write(line)
                else:
                    continue
            else:
                continue
        f.close()
    out.close() 

