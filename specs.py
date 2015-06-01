import csv
import sys
import os

def readFile(fileIN,delim):								## Boiler Plate File reader
	Data={}
	with open(fileIN) as csvfile:
		tableMain=csv.reader(csvfile,delimiter=delim)
		k=0
		for row in tableMain:
			Data[k]=row
			k+=1							
	return(Data)

def GetItNumeric(location):
	if(location[len(location)-4:len(location)]!=".csv"):
		location+=".csv"
	data = readFile(location,",")
	samples = 96.*96.
	nonZeroSamples = 0.
	mean = 0.
	minimum = None
	maximum = None
	for k in range(1,len(data)):						## Mean E[x] where x is a random sample from qpcr reads
		for k2 in range(1,len(data[k])):
			mean+=float(data[k][k2])
			if(float(data[k][k2])!=0.):
				nonZeroSamples += 1.
				if(minimum > float(data[k][k2]) or minimum == None):
					minimum = float(data[k][k2])
				if(maximum < float(data[k][k2]) or maximum == None):
					maximum = float(data[k][k2])
	nonZeroMean = mean/nonZeroSamples
	mean = mean/samples
	frequency = nonZeroSamples/samples
	std = 0.											## Standard deviation calculation (E[(mu-X)^2)])^0.5
	nzStd = 0.
	for k in range(1,len(data)):
		for k2 in range(1,len(data[k])):
			std+=(float(data[k][k2])-mean)**2
			if(float(data[k][k2])!=0):
				nzStd+=(float(data[k][k2])-nonZeroMean)**2
	std = (std/samples)**0.5
	nzstd = (nzStd/nonZeroSamples)**0.5
	return({"Frequency":frequency,"nzMean":nonZeroMean,"Mean":mean,"nzstd":nzstd,"std":std,"Minimum":minimum,"Maximum":maximum})

def getItAll(location):
	data = readFile(location+"F0.csv",",")
	calls = readFile(location+"Qc.csv",",")
	Fpos=0
	Fneg=0
	Tpos=0
	Tneg=0
	Avg = 0
	std = 0
	passing = 0
	passingAvg=0
	samples = 96**2
	for k in range(1,len(data)):
		for k2 in range(1,len(data[k])):
			if(data[k][k2]==""):
				data[k][k2]="0"
			if(calls[k][k2]=="Pass"):
				Avg+=float(data[k][k2])
				if(float(data[k][k2])!=0):
					Tpos+=1
				else:
					Tneg+=1
			else:
				if(float(data[k][k2])!=0):
					Fpos+=1
				else:
					Fneg+=1
	Avg=Avg/Tpos
	for k in range(1,len(data)):
		for k2 in range(1,len(data[k])):
			if(float(data[k][k2])!=0. and calls[k][k2]=="Pass"):
				std+=(float(data[k][k2])-Avg)**2
	std	= (std/Tpos)**0.5
	return({"True Positive":Tpos,"True Negitive":Tneg,"False Positive":Fpos, "False Negitive": Fneg,"Mean":Avg,"Standard Deviation" : std})

def dcPrint(dic,loc):
	ret = ""
	for k in dic:
		ret += k +"\t:\t" + str(dic[k]) + "\n"
	f = open(loc,"w")
	f.write(ret)
	f.close()

def goHAM(Folder):
	os.chdir(Folder)
	gia = ["m5","m6","m8"]
	gin = ["m1F0","m1Qc"]
	for k in gin:
		print(k)
		dcPrint(GetItNumeric(k),k+"Specs.csv")
	for k in gia:
		print(k)
		dcPrint(getItAll(k),k+"Specs.csv")

target = sys.argv[1]
goHAM(target)
