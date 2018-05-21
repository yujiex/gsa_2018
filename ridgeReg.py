from sklearn.linear_model import Ridge
from sklearn.linear_model import RidgeCV
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import train_test_split
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import pylab as P
import seaborn as sns

import util_io as uo
import lean_temperature_monthly as ltm

my_dpi = 300
imagedir = '/media/yujiex/work/project/images/'

def mse(y, yhat):
    return sum((y - yhat)**2)/len(y)
    
def load_data(b, measure_type):
    conn = uo.connect('interval_ion')
    with conn:
        df = pd.read_sql('SELECT * FROM {0}_wtemp WHERE Building_Number = \'{1}\''.format(measure_type, b), conn)
    num = 720
    df['chunk'] = df.index/num
    
    df = df.groupby('chunk').filter(lambda x: len(x) == num)
    y = np.array(df.groupby('chunk').sum()['eui'])
    X = df.groupby('chunk')['Temperature_F'].apply(list).tolist()
    X = np.array(X)
    [Xtrain, Xtest, ytrain, ytest] = \
        train_test_split(X, y, test_size = 0.3, random_state=0)
    return [Xtrain, Xtest, ytrain, ytest]
    
# add cv later
def baseline_test():
    b = 'OR0033PE'
    s = 'KPDX'
    npar = 3
    [Xtrain, Xtest, ytrain, ytest] = load_data(b, 'gas')
    tmean = np.mean(Xtrain, axis=1)
    d = ltm.piecewise_reg_one(b, s, npar, 'eui_gas', False, None, x=tmean, y=ytrain)
    yhat = d['fun'](np.mean(Xtest, axis=1), *d['regression_par'])
    error = mse(ytest, yhat)
    print error

def ridge_test():
    [Xtrain, Xtest, ytrain, ytest] = load_data('OR0033PE', 'gas')
    # quesion: how to auto-select the lambda (fixme)

    lambdas = np.arange(1, 1000000, 1000)
    errs = []
    # lambdas = lambdas[:2]
    # for x in lambdas:
    #     alpha = x
    #     clf = Ridge(alpha=alpha)
    #     clf.fit(Xtrain, ytrain)
    #     m = Ridge(alpha=alpha, copy_X=True, fit_intercept=True, max_iter=None,
    #         normalize=False, random_state=None, solver='auto', tol=0.001)
    #     scores = cross_val_score(clf, Xtrain, ytrain, cv=5, scoring='neg_mean_squared_error')
    #     # R2 error
    #     # scores = cross_val_score(clf, Xtrain, ytrain, cv=5)
    #     error = abs(scores.mean())
    #     errs.append(error)
    # plt.xlabel('lambda (ridge term)')
    # plt.ylabel('mse (mean squared error)')
    # plt.title('error change with change of ridge term')
    # plt.plot(lambdas, errs, 'o')
    # path = imagedir + 'cv_5_fold_ridge.png'
    # P.savefig(path, dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi))
    # plt.close()

    # try using sklearn.linear_model.RidgeCV
    # lambdas = np.arange(1, 1000000, 1000)
    # errs = []
    # clf = RidgeCV().fit(Xtrain, ytrain)
    # RidgeCV(alphas=np.arange(1, 1000000, 1000), fit_intercept=True,
    #         normalize=False, store_cv_values=True)
    # yhat = m.predict(Xtest)
    # error = mse(y, yhat)
    # print error

    # gather best ridge term and re-fit
    alpha = 190000
    clf = Ridge(alpha=alpha)
    clf.fit(Xtrain, ytrain)
    Ridge(alpha=alpha, copy_X=True, fit_intercept=True, max_iter=None,
          normalize=False, random_state=None, solver='auto', tol=0.001)
    scores = cross_val_score(clf, Xtrain, ytrain, cv=5, scoring='neg_mean_squared_error')
    yhat = clf.predict(Xtest)
    params = clf.coef_
    plt.plot(params)
    # plt.show()
    plt.xlabel('nth')
    plt.title('plot of ridge model coefficients')
    path = imagedir + 'param_ridge_opt.png'
    P.savefig(path, dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi))
    plt.close()
    error = mse(ytest, yhat)
    print error
    # print scores
    print 'end'
    return
    
def main():
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1)
    # ridge_test()
    baseline_test()
    return 0
    
main()
