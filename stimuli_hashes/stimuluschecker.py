import os, csv

#Gets locations of all stimulus log files
RAW_DIR = os.getcwd() + "/../ground_truth_test/"
NEW_SUBDIR = []
STIM_LOG_LOCATIONS = []

for file in os.listdir(RAW_DIR):
	if "log" in file:
		STIM_LOG_LOCATIONS.append(RAW_DIR + "/" + file)

#gets the hashes from stimulus log files
hash_key_dict = {}
hash_key_list = []
for stimuluslog in STIM_LOG_LOCATIONS:
	with open(stimuluslog) as stimfile:
		for line in stimfile:
			temp_key = ''
			temp_hash = ''
			temp_line = line.split(' ')
			for item in temp_line:
				if "Name" in item:
					temp_key = item.split('=')[1].strip('"')
				if "StimulusFile" in item:
					temp_hash = item.split('=')[1].strip('"')
			hash_key_list.append([temp_key,temp_hash])
			hash_key_dict[temp_key] = temp_hash

# print (hash_key_dict)
# print (hash_key_list)


#write ALL hashkeys (from hash_key_list) to csv
with open('allhashes.csv','w',newline='') as csv_out:
	writer = csv.writer(csv_out, dialect = 'excel')
	for pair in hash_key_list:
		writer.writerow([pair[0],pair[1]])

#write UNIQUE hashkeys (hash_key_dict) to csv
#make sure that hashkeys between files are the same for each video
with open('uniquehashes.csv','w',newline='') as csv_out:
	writer = csv.writer(csv_out, dialect = 'excel')
	for key in hash_key_dict:
		writer.writerow([key,hash_key_dict[key]])

for key in hash_key_dict:
	print("raw_data[raw_data$stimulus == '" + hash_key_dict[key] + "','stimulus'] <- '"+ key + "'")
