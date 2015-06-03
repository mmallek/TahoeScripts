Script needs

Open "wunder-data-(date)"

#If "TimePDT" like "10:** AM"
    #Write "Wind Direction", "Wind SpeedMPH", "GustSpeedMPH", "WindDirDegrees"
    #from that line to the file "wind-data"
    #Add tag at beginning or end of line with date? Or number somehow. Would 
    #be nice to know that each value is unique.

Want to loop through times from "10:** AM" hourly through/including "6:** PM", 
    so may need a list of eligible values. Need to ensure that minutes do not
    matter, as they are inconsistent.


Later

Will need to take "Wind Direction" or "WindDirDegrees" and sort them into classes
Then calculate relative proportion in each class over all time.

Will need to take "wind SpeedMPH" and average over all time (but not by direction(?))

import glob

def wunderclean():
    outname = '../Climate/WeatherStationData/wunderclean.txt'
    out = open(outname, 'w')
    for fname in glob.glob('../Climate/WeatherStationData/wunder-data-*.txt'):
        f = open(fname, 'r')
        flines = f.readlines()
        for line in flines:
            if line.startswith('TimePDT') or line == '':
                continue
            hour = int(line.split(':')[0])
            elif 10 <= hour < 12 and 'AM' in line:
                out.write(line)
            elif 12 == hour and 'PM' in line:
                out.write(line)
            elif 1 <= hour <= 6 and 'PM' in line:
                out.write(line)
        f.close()
    out.close()




