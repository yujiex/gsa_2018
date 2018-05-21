import pandas as pd
import numpy as np
from sklearn.cluster import DBSCAN
from sklearn import metrics
from sklearn.datasets.samples_generator import make_blobs
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
import pylab as P
import os
my_dpi = 100

def plot_cluster(df, b, s, measure_type, e, n_sample):
    df = df[['Temperature_F', 'eui']]
    df = df[df['eui'] >= 0]
    up = df['eui'].quantile(0.99)
    df = df[df['eui'] < up]
    X = df.as_matrix() 
    X = StandardScaler().fit_transform(X)

    # Compute DBSCAN
    db = DBSCAN(eps=e, min_samples=n_sample).fit(X)
    core_samples_mask = np.zeros_like(db.labels_, dtype=bool)
    core_samples_mask[db.core_sample_indices_] = True
    labels = db.labels_
    print set(labels)
    print (labels[:10])

    # Number of clusters in labels, ignoring noise if present.
    n_clusters_ = len(set(labels)) - (1 if -1 in labels else 0)

    unique_labels = set(labels)
    colors = plt.cm.Spectral(np.linspace(0, 1, len(unique_labels)))
    for k, col in zip(unique_labels, colors):
        if k == -1:
            # Black used for noise.
            col = 'k'

        class_member_mask = (labels == k)

        xy = X[class_member_mask & core_samples_mask]
        plt.plot(xy[:, 0], xy[:, 1], 'o', markerfacecolor=col,
                markersize=6)
                # markeredgecolor='k', markersize=14)

        xy = X[class_member_mask & ~core_samples_mask]
        plt.plot(xy[:, 0], xy[:, 1], 'o', markerfacecolor=col,
                markersize=2)
                # markeredgecolor='k', markersize=6)
        plt.ylim((-2, 10))

    plt.title('Cluster of {0}'.format(b))
    # plt.show()
    path = os.getcwd() + '/input/FY/interval/ion_0627/{0}_scatter/{1}_{2}_{3}'.format(measure_type, b, s, 'cluster')
    P.savefig(path, dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi), bbox_inches='tight')
    plt.close()
