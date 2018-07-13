#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov  2 09:11:30 2017

Submitted by: Tamara Williams

Based on material from the O'Reilly book: 
Hands-on Machine Learning with Scikit-Learn and TensorFlow (chapter 10)
by Aurélien Geron
"""



# import the packages we will need 
import time
#from datetime import datetime  # use for time-stamps in activity log
import numpy as np
import matplotlib.pyplot as plt
import tensorflow as tf
from sklearn.metrics import confusion_matrix, f1_score
import seaborn as sns

# get the MNIST dataset 
#----------------------------------------------------------
from tensorflow.examples.tutorials.mnist import input_data

mnist = input_data.read_data_sets("/tmp/data/")


X_train = mnist.train.images
X_test = mnist.test.images
y_train = mnist.train.labels.astype("int")
y_test = mnist.test.labels.astype("int")

############################
n_inputs = 28*28  # MNIST

#n_hidden = 10
n_hidden = 50
n_hidden1= n_hidden2=n_hidden3=n_hidden4=n_hidden5 = n_hidden  
n_outputs = 10
n_layers = 5
#n_layers = 2

#n_hidden1 = 50     #300 - was default in sample in book
#n_hidden2 = 40     #100 - default in sample in book
#n_hidden3 = 30  
#n_hidden4 = 20  
#n_hidden5 = 10

tf.reset_default_graph

X = tf.placeholder(tf.float32, shape=(None, n_inputs), name="X")
y = tf.placeholder(tf.int64, shape=(None), name="y")

#define model
#----------------------------------------------------
def neuron_layer(X, n_neurons, name, activation=None):
    with tf.name_scope(name):
        n_inputs = int(X.get_shape()[1])
        stddev = 2 / np.sqrt(n_inputs)
        init = tf.truncated_normal((n_inputs, n_neurons), stddev=stddev)
        W = tf.Variable(init, name="kernel")
        b = tf.Variable(tf.zeros([n_neurons]), name="bias")
        Z = tf.matmul(X, W) + b
        if activation is not None:
            return activation(Z)
        else:
            return Z
        
        
with tf.name_scope("dnn"):
    hidden1 = neuron_layer(X, n_hidden1, name="hidden1",
                           activation=tf.nn.relu)
    hidden2 = neuron_layer(hidden1, n_hidden2, name="hidden2",
                           activation=tf.nn.relu)
    hidden3 = neuron_layer(hidden2, n_hidden3, name="hidden3",
                           activation=tf.nn.relu)
    hidden4 = neuron_layer(hidden3, n_hidden4, name="hidden4",
                           activation=tf.nn.relu)
    hidden5 = neuron_layer(hidden4, n_hidden5, name="hidden5",
                         activation=tf.nn.relu)
    logits = neuron_layer(hidden5, n_outputs, name="outputs")        
#    logits = neuron_layer(hidden2, n_outputs, name="outputs")       
        

with tf.name_scope("loss"):
    xentropy = tf.nn.sparse_softmax_cross_entropy_with_logits(labels=y,
                                                              logits=logits)
    loss = tf.reduce_mean(xentropy, name="loss")


learning_rate = 0.01

with tf.name_scope("train"):
    optimizer = tf.train.GradientDescentOptimizer(learning_rate)
    training_op = optimizer.minimize(loss)


with tf.name_scope("eval"):
    correct = tf.nn.in_top_k(logits, y, 1)
    accuracy = tf.reduce_mean(tf.cast(correct, tf.float32))
    

init = tf.global_variables_initializer()
saver = tf.train.Saver()


n_epochs = 40
batch_size = 50

# train model
#----------------------------------------------------
start_time = time.time()
with tf.Session() as sess:
    init.run()
    for epoch in range(n_epochs):
        for iteration in range(mnist.train.num_examples // batch_size):
            X_batch, y_batch = mnist.train.next_batch(batch_size)
            sess.run(training_op, feed_dict={X: X_batch, y: y_batch})
        acc_train = accuracy.eval(feed_dict={X: X_batch, y: y_batch})
        acc_val = accuracy.eval(feed_dict={X: mnist.validation.images,
                                            y: mnist.validation.labels})
        print(epoch, "Train accuracy:", acc_train, "Val accuracy:", acc_val)

    save_path = saver.save(sess, "/tmp/my_model_v1.ckpt")
build_time = time.time() - start_time



# execution phase
start_time = time.time()
with tf.Session() as sess:
    saver.restore(sess, "/tmp/my_model_v1.ckpt") # or better, use save_path
#    X_new_scaled = mnist.test.images[:20]
    X_new_scaled = mnist.test.images
    Z = logits.eval(feed_dict={X: X_new_scaled})
    y_pred = np.argmax(Z, axis=1)
predict_time = time.time() - start_time


print("Predicted classes:", y_pred)
print("Actual classes (first 20):   ", mnist.test.labels[:20], '\n\n')  
print("Results for %s layers, with %s nodes per layer" %(n_layers, n_hidden))
print("Training Accuracy:  ", acc_train)
print("Test Accuracy:   ",  acc_val)
print("F Score:", f1_score(mnist.test.labels, y_pred, average='micro'))
print("Total Time:   ", predict_time + build_time)

#print graphical version of results 
conf_mat=confusion_matrix(mnist.test.labels, y_pred)

# visualize results
fig = plt.figure()
ax = fig.add_subplot(111)
sns.heatmap(conf_mat, annot = True, annot_kws={'size': 9}, fmt = 'd', cmap="YlGnBu")# font size
plt.ylabel('Actual')
plt.xlabel('Predicted')
chart_name = "Actual versus Predicted for " + str(n_layers)+" layers "+str(n_hidden)+" nodes"
save_name = "TF_res_"+str(n_layers)+'x'+str(n_hidden)
plt.title(chart_name)
plt.savefig(save_name, 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  
plt.show()



        
