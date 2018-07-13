#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov  2 11:50:17 2017

Submitted by: Tamara Williams

Based on material from the O'Reilly book: 
Hands-on Machine Learning with Scikit-Learn and TensorFlow (chapter 10)
by Aurélien Geron
"""
#n_layers = 5
n_layers = 2
#n_hidden = 10
n_hidden = 50


from tensorflow.examples.tutorials.mnist import input_data
import tensorflow as tf
import time
from sklearn.metrics import confusion_matrix, accuracy_score, log_loss, f1_score
import seaborn as sns
import matplotlib.pyplot as plt


mnist = input_data.read_data_sets("/tmp/data/")
X_train = mnist.train.images
X_test = mnist.test.images
y_train = mnist.train.labels.astype("int")
y_test = mnist.test.labels.astype("int")



start_time = time.time()
config = tf.contrib.learn.RunConfig(tf_random_seed=13) # not shown in the config

feature_cols = tf.contrib.learn.infer_real_valued_columns_from_input(X_train)
#dnn_clf = tf.contrib.learn.DNNClassifier(hidden_units=[n_hidden,n_hidden, 
#                                n_hidden, n_hidden, n_hidden], n_classes=10,
#                                feature_columns=feature_cols, config=config)
dnn_clf = tf.contrib.learn.DNNClassifier(hidden_units=[n_hidden,n_hidden], 
                    n_classes=10, feature_columns=feature_cols, config=config)
dnn_clf = tf.contrib.learn.SKCompat(dnn_clf) # if TensorFlow >= 1.1
dnn_clf.fit(X_train, y_train, batch_size=50, steps=40000)
build_time = time.time()-start_time

y_pred_train = dnn_clf.predict(X_train)
train_acc = accuracy_score(y_train, y_pred_train['classes'])

start_time = time.time()
y_pred = dnn_clf.predict(X_test)
test_acc = accuracy_score(y_test, y_pred['classes'])
predict_time = time.time() - start_time

y_pred_proba = y_pred['probabilities']
log_loss(y_test, y_pred_proba)

total_time = predict_time+build_time


print('\n\n')
print("Results for %s layers, with %s nodes per layer" %(n_layers, n_hidden))
print("Training Accuracy:  ", train_acc)
print("Test Accuracy:    ",accuracy_score(y_test, y_pred['classes']))
print("F Score:", f1_score(y_test, y_pred['classes'], average='micro'))
print("Total Time:   ", total_time)

       
#print graphical version of results 
conf_mat=confusion_matrix(y_test, y_pred['classes'])

# visualize results
fig = plt.figure()
ax = fig.add_subplot(111)
sns.heatmap(conf_mat, annot = True, annot_kws={'size': 9}, fmt = 'd', cmap="YlGnBu")# font size
plt.ylabel('Actual')
plt.xlabel('Predicted')
chart_name = "Tensorflow API thru Scikit Actual versus Predicted for " + str(n_layers)+" layers "+str(n_hidden)+" nodes"
save_name = "TF_Sk_res_"+str(n_layers)+'x'+str(n_hidden)
plt.title(chart_name)
plt.savefig(save_name, 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  
plt.show()