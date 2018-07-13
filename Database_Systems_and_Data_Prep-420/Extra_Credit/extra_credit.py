# Tamara Williams
# Extra Credit - 420, Sec 57, Spring 2017
import string
import re

# init a list, open the text file and read line-by-line
word_list = []
with open('babble-words.txt') as input_file:
    for line in input_file:
        # convert to all lower case
        line = line.lower()
        # get rid of that pesky punctuation
        new_line =  re.sub('[%s]' % string.punctuation,'', line)
        # and get rid of the annoying new line characters
        new_line =  re.sub('\n','', new_line)
        # split into words breaking at the spaces
        words = new_line.split(' ')
        # append each word to the new list
        word_list.append(words)

#flatten the list
flat_list=[]
for wrd in word_list:
    for w in wrd:
        flat_list.append(w)
#print flat_list

# use the set function to return the unique list of words
unique_words = set(flat_list)
#init a dict to hold results
count_dict = {}
# loop thru the clean text and count the occurances, saving results to dict
for word in unique_words:
    number = flat_list.count(word)
    count_dict.update({word: flat_list.count(word)})

#sort the dictionary
sorted_dict = sorted(count_dict.items(), key=lambda kv: kv[1], reverse=True)
# iterate thru the first 10 items in the sorted dictionary to get top 10
for i in range(0,10):
    print sorted_dict[i]
