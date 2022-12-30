#R CODE
#FIRST ID# 1’s place - indicates section
0.xx - *Reserved for master script****
1.xx	survey - profiles
2.xx	survey - nutrients
3.xx	survey - phyto
4.xx	long-term survey
5.xx	buoy - sensors
6.xx	metabolism
7.xx      (nothing… bathymetry? zoop?)
8.xx	combined methods
9.xx	junk. color codes, date.time and other references. 

#SECOND ID# first decimal - indicates code function
x.01 	load data. ALL database modifications should take place here and only here! (i know it’s not the first decimal but this made sense to me)
x.1x 	summary stats, more stats, etc. from x.10 to x.19. 
X.2x	figures. (Maybe try to organize each set of ten by subject i guess, or just go in order using name)

#THIRD ID# second decimal - just a number
x.10	stats item 1
x.13	another stats item 
5.22	buoy fig 3
8.24	combo fig 5
