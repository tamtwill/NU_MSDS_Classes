#!/usr/bin/env python2
# -*- coding: utf-8 -*-

## Graded Exercise 2
## Tamara Williams

import pandas as pd
import sqlite3
import pickle
import random

## For the data files (mail, item and customer), read into a dataframe.  
## Use an empty string to fill missing data.  
## No path variable or relatives paths are used; assumes data is in local path

df_mail = pd.read_csv("mail_tst.csv", sep = ",")
df_mail = df_mail.fillna('')
print df_mail.head(5),"\n"
df_item = pd.read_csv("items_tst.csv", sep = ",")
df_item = df_item.fillna('')
df_cust = pd.read_csv("customers_tst.csv", sep = ",")
df_cust = df_cust.fillna('')

# Count the number of duplicate customer records, where "duplicate" means all
# fields except rownum are the same
dup_cust = df_cust.duplicated().sum()
print "There were", dup_cust, "duplicate Customer records found\n"

#create a list of the unique customer account numbers
cust_acc = list(df_cust['acctno'].unique())

## Since we are looking for records that are in df_mail, but NOT in df_cust
## use the pd.isin() function, set to '==False' for records where the value of
## df_mail.acctno is not in the list of unique Customer acctno's.  
## Repeat for df_item.
missing_mail = df_mail[df_mail.acctno.isin(cust_acc)==False]
missing_item = df_item[df_item.acctno.isin(cust_acc)==False]
print "There are ", len(missing_mail)," account numbers in Mail not in Customers"
print "There are ", len(missing_item)," account numbers in Item not in Customers"
## filter the dataframe to remove items that are in mail or item and not in customer, 
## save cleaned dataframe to a new name for clarity, and to use importing into the SQLDB
clean_mail = df_mail[df_mail.acctno.isin(missing_mail['acctno'])==False]
clean_item = df_item[df_item.acctno.isin(missing_item['acctno'])==False]

#==============================================================================
# #testing missing acount numbers, check doing another way. 
# #for missing = 0 this will match missingMail, if there are duplicate
# #entries in df_mail for missing customer accounts, this will not match above
# mailAcc = list(df_mail['acctno'].unique()) 
# itemAcc = list(df_item['acctno'].unique())
# missingMailChk = list(set(mailAcc) - set(custAcc))
# missingItemChk = list(set(itemAcc) - set(custAcc))
# print missingMailChk
# print missingItemChk
#==============================================================================

## de-dup the mail and items frames
#mail_before = len(df_mail)
#item_before = len(df_item)
#df_mail = df_mail.drop_duplicates()
#df_item = df_item.drop_duplicates()
#mail_after = len(df_mail)
#item_after = len(df_item)
#print mail_before - mail_after, "duplicate rows removed from mail, and", item_before-item_after,\
#"duplicate rows removed from item\n"


## Check for database, and if it exists, delete it so can recreate clean
import os
import errno
try:
    os.remove('XYZ_DB.db')
except OSError as e:
    if (e == 2):
        pass

#Create/connect to the SQLite3 database and import the dataframes
db_conn = sqlite3.connect('XYZ_DB.db')
clean_mail.to_sql("mail", db_conn, if_exists='replace')
clean_item.to_sql("item", db_conn, if_exists="replace")
df_cust.to_sql("customers", db_conn, if_exists='replace')
db_conn.commit()
db_conn.close()

# verify 1) there is data in the tables and 2) the dataframe length matches 
# the number of rows in the table 3) that a random row is the same between the 
# dataframe and table adjusting for index differences. 
db_conn = sqlite3.connect('XYZ_DB.db')
c = db_conn.cursor()

def check_table(df, str_table):
    tmp_var = '--tempVar--'
    r_count = 0
    q1 = 'select * from tmp_var'.replace('tmp_var', str_table)
    for row in c.execute (q1):
        r_count +=1
    i = random.randint(1, len(df))
    q2 = 'select rowid, * from tmp_var where rowid = ?'.replace('tmp_var', str_table)
    q_cursor = c.execute(q2, [i])
    s_res = q_cursor.fetchone()
    d_res = df.ix[i-1]
    if (s_res[2] == d_res.acctno) and (r_count == len(df)):
        res_str = "Length and random row match, tmp_var table\
            is good".replace('tmp_var', str_table)
        print res_str
    else:
        res_str = """Length and random row match, tmp_var table don't match, 
            there was an error""". replace('tmp_var', str_table)
        print res_str
    
print '\n'
print "******* Verifying Tables ********"
check_table(clean_mail, 'mail')
check_table(clean_item, 'item')
check_table(df_cust,'customers')
#
##==============================================================================
## rcMail = 0    
## for row in c.execute("select * from mail"):
##     rcMail +=1
## i = random.randint(1, len(df_mail))
## select_row_query = ' select ... '.replace('##TABLE_NAME##', table_name)
## q = 'SELECT rowid, * FROM ' + table_name + ' where rowid = ?'
## mycursor = c.execute(q, [i])
## s = mycursor.fetchone()
## d = df_mail.ix[i-1]
## if (s[2] == d.acctno) and (rcMail == len(df_mail)):
##     print "Length and random row match, mail table is good"
## else:
##     print "There was a problem with mail table creation"
## 
## rcItem = 0   
## for row in c.execute("select * from item"):
##     rcItem +=1
## i = random.randint(1, len(df_item))
## q = 'SELECT rowid, * FROM item where rowid = ?'
## mycursor = c.execute(q, [i])
## s = mycursor.fetchone()
## d = df_item.ix[i-1]
## if (s[2] == d.acctno) and (rcItem == len(df_item)):
##     print "Length and random row match, item table is good"
## else:
##     print "There was a problem with item table creation"
##  
## rcCust = 0   
## for row in c.execute("select * from customers"):
##     rcCust +=1
## i = random.randint(1, len(df_cust))
## q = 'SELECT rowid, * FROM customers where rowid = ?'
## mycursor = c.execute(q, [i])
## s = mycursor.fetchone()
## d = df_cust.ix[i-1]
## if (s[2] == d.acctno) and (rcCust == len(df_cust)):
##     print "Length and random row match, customer table is good"
## else:
##     print "There was a problem with customer table creation"
##==============================================================================


## create a new table with the customers who have been mailed 7 or more times
c.execute("""
    create table if not exists mailed as 
    select acctno, 
    (mail_1+mail_2+mail_3+mail_4+mail_5+mail_6+mail_7+mail_8+mail_9+
    mail_10+mail_11+mail_12+mail_13+mail_14+mail_15+mail_16) as total_mailed 
    from mail
    where total_mailed >= 7;""")

## create the table for customers to target, as customers who have been 
## mailed 7+ times, with desired columns from customers table by acctno
c.execute("""
    create table if not exists target as 
    select mailed.acctno, total_mailed, YTD_TRANSACTIONS_2009, YTD_SALES_2009,
      ZHOMEENT, ZMOBAV, ZCREDIT, ZHITECH 
    from mailed, customers
    where mailed.acctno = customers.acctno;""")

#add the new columns for coding into, then perform the coding.  My understanding
#of the task is an empty field get no code in the new columns.
c.execute("""alter table target add column ZHOMEENT01 int;""") 
c.execute("""alter table target add column ZMOBAV01 int;""")
c.execute("""alter table target add column ZCREDIT01 int;""")
c.execute("""alter table target add column ZHITECH01 int;""")
c.execute("""update target set ZHOMEENT01 = (case when ZHOMEENT = "Y" then 1 when (ZHOMEENT <> "Y" and (ZHOMEENT <> "" or ZHOMEENT <> null)) then 0 end);""")
c.execute("""update target set ZMOBAV01 = (case when ZMOBAV = "Y" then 1 when (ZMOBAV <> "Y" and (ZMOBAV <> "" or ZMOBAV <> null)) then 0 end);""")
c.execute("""update target set ZCREDIT01 = (case when ZCREDIT = "Y" then 1 when (ZCREDIT <> "Y" and (ZCREDIT <> "" or ZCREDIT <> null)) then 0 end);""")
c.execute("""update target set ZHITECH01 = (case when ZHITECH = "Y" then 1 when (ZHITECH <> "Y" and (ZHITECH <> "" or ZHITECH <> null)) then 0 end);""")
#perform a visual inspection of correctness since the page limit prevents much else
print '\n'
print "****** Visually inspect coding results ****"
print "Correct: 'Y' -> 1, blank or null -> None, all else -> 0"
for row in c.execute("select * from target limit 15"):
    print row
print '\n'

#write the target table as a CSV file
write_table = pd.read_sql_query('select * from target',db_conn)
write_table.to_csv('target.csv', header=True, index=False)

# do the cross-tabs for each z-pair, print as we go
print pd.crosstab(write_table.zhomeent, write_table.ZHOMEENT01,margins = True), '\n'
print pd.crosstab(write_table.zmobav, write_table.ZMOBAV01,margins = True), '\n'
print pd.crosstab(write_table.zcredit, write_table.ZCREDIT01,margins = True), '\n'
print pd.crosstab(write_table.zhitech, write_table.ZHITECH01,margins = True), '\n'


#Commit the changes to the DB and close the connection to save to local folder
db_conn.commit()
db_conn.close()

## pickle mail, item and customer dataframes, stored in local working directory
clean_mail.to_pickle('XYZ_mail.pkl')
clean_item.to_pickle('XYZ_item.pkl')
df_cust.to_pickle('XYZ_cust.pkl')
