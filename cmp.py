import sklearn.linear_model as lm
import sklearn.model_selection as ms
from sklearn import preprocessing
from sklearn.decomposition import PCA
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import pylab as P
import seaborn as sns

import util_io as uo
import lean_temperature_monthly as ltm
import pca_util

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
        ms.train_test_split(X, y, test_size = 0.3, random_state=0)
    return [Xtrain, Xtest, ytrain, ytest]
    
# add cv later
def baseline_ordinary(b, measure_type):
    [Xtrain, Xtest, ytrain, ytest] = load_data(b, measure_type)
    tmean = np.mean(Xtrain, axis=1)
    lr = lm.LinearRegression()
    lr.fit(Xtrain, ytrain)
    yhat = lr.predict(Xtest)
    error = mse(ytest, yhat)
    print error

def baseline_piecewise(b, measure_type, s):
    npar = 3
    [Xtrain, Xtest, ytrain, ytest] = load_data(b, measure_type)
    tmean = np.mean(Xtrain, axis=1)
    if measure_type == 'gas':
        d = ltm.piecewise_reg_one(b, s, npar, 'eui_gas', False, None, x=tmean, y=ytrain)
    else:
        d = ltm.piecewise_reg_one(b, s, npar, 'eui_elec', False, None, x=tmean, y=ytrain)
    yhat = d['fun'](np.mean(Xtest, axis=1), *d['regression_par'])
    error = mse(ytest, yhat)
    print error

def ridge_test(b, measure_type):
    [Xtrain, Xtest, ytrain, ytest] = load_data(b, measure_type)
    # quesion: how to auto-select the lambda (fixme)

    lambdas = np.arange(1, 1000000, 1000)
    errs = []
    # lambdas = lambdas[:2]
    # for x in lambdas:
    #     alpha = x
    #     clf = lm.Ridge(alpha=alpha)
    #     clf.fit(Xtrain, ytrain)
    #     m = lm.Ridge(alpha=alpha, copy_X=True, fit_intercept=True, max_iter=None,
    #         normalize=False, random_state=None, solver='auto', tol=0.001)
    #     scores = ms.cross_val_score(clf, Xtrain, ytrain, cv=5, scoring='neg_mean_squared_error')
    #     # R2 error
    #     # scores = ms.cross_val_score(clf, Xtrain, ytrain, cv=5)
    #     error = abs(scores.mean())
    #     errs.append(error)
    # plt.xlabel('lambda (ridge term)')
    # plt.ylabel('mse (mean squared error)')
    # plt.title('error change with change of ridge term')
    # plt.plot(lambdas, errs, 'o')
    # path = imagedir + 'cv_5_fold_ridge.png'
    # P.savefig(path, dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi))
    # plt.close()

    # try using sklearn.linear_model.lm.RidgeCV
    # lambdas = np.arange(1, 1000000, 1000)
    # errs = []
    # clf = lm.RidgeCV().fit(Xtrain, ytrain)
    # lm.RidgeCV(alphas=np.arange(1, 1000000, 1000), fit_intercept=True,
    #         normalize=False, store_cv_values=True)
    # yhat = clf.predict(Xtest)
    # error = mse(y, yhat)
    # print error

    # gather best ridge term and re-fit
    alpha = 190000
    clf = lm.Ridge(alpha=alpha)
    clf.fit(Xtrain, ytrain)
    lm.Ridge(alpha=alpha, copy_X=True, fit_intercept=True,
             max_iter=None, normalize=False, random_state=None,
             solver='auto', tol=0.001)
    scores = ms.cross_val_score(clf, Xtrain, ytrain, cv=5, scoring='neg_mean_squared_error')
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
    # print 'end'
    return error
    
def pca_test_raw(b, measure_type):
    [Xtrain, Xtest, ytrain, ytest] = load_data(b, measure_type)
    var, pcs = pca_util.get_pc_matrix(Xtrain)
    errs = []
    total_vars = []
    accounted_vars = np.cumsum(var) / sum(var)
    # print accounted_vars
    # plt.plot(range(1, len(var) + 1), accounted_vars, 'o')
    # plt.title('Accounted variances vs #pcs')
    # plt.xlabel('# principal components')
    # plt.ylabel('percent of variances accounted for')
    # path = imagedir + 'pca_err_numpc_{0}.png'.format(b)
    # P.savefig(path, dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi))
    for pc in enumerate(pcs):
        print i

def pca_test(b, measure_type):
    [Xtrain, Xtest, ytrain, ytest] = load_data(b, measure_type)

    # standardize training data
    scaler_x = preprocessing.StandardScaler().fit(Xtrain)
    Xtrain_scale = scaler_x.transform(Xtrain)
    Xtest_scale = scaler_x.transform(Xtest)
    scaler_y = preprocessing.StandardScaler().fit(ytrain.reshape(-1, 1))
    ytrain_scale = scaler_y.transform(ytrain.reshape(-1, 1))
    ytest_scale = scaler_y.transform(ytest.reshape(-1, 1))
    # print Xtrain_scale.std(axis=0)

    # Xtrain_scale = Xtrain
    num_comps = range(1, ytest.shape[0])
    errs = []
    total_vars = []
    for num_comp in num_comps:
        pca = PCA(n_components=num_comp)
        pca.fit(Xtrain_scale)
        PCA(copy=True, iterated_power='auto', n_components=num_comp,
            random_state=None, svd_solver='auto', tol=0.0, whiten=False)
        total_var = sum(pca.explained_variance_ratio_)
        total_vars.append(total_var)
        pcs = pca.components_
        Xtrain_trans = pca.fit_transform(Xtrain_scale)

        lr = lm.LinearRegression()
        lr.fit(Xtrain_trans, ytrain_scale)
        Xtest_trans = pca.fit_transform(Xtest_scale)
        yhat_scale = lr.predict(Xtest_trans)

        # line1, = plt.plot(yhat, 'o')
        # line2, = plt.plot(ytest, 'o')
        # plt.legend([line1, line2], ['yhat', 'y'], loc=2,
        #            bbox_to_anchor=(1, 1))
        # plt.show()

        yhat = scaler_y.inverse_transform(yhat_scale)
        error = mse(ytest_scale, yhat)[0]
        errs.append(error)
        print num_comp, total_var, error
    f, axarr = plt.subplots(2, sharex=True)
    axarr[0].plot(num_comps, errs, '-o', c='blue')
    axarr[0].set_title('MSE changing as number of PC increases')
    axarr[1].plot(num_comps, total_vars, '-o', c='red')
    axarr[1].set_title('Accounted variance ratio')
    plt.xlabel('number of PC (Principal Components) increases')
    # plt.show()
    path = imagedir + 'pca_err_pc.png'
    P.savefig(path, dpi = my_dpi, figsize = (2000/my_dpi, 500/my_dpi))
    plt.close()
    
def main():
    sns.set_style("whitegrid")
    sns.set_context("talk", font_scale=1)
    b = 'OR0033PE'
    s = 'KPDX'
    measure_type = 'gas'
    # baseline_ordinary(b, measure_type)
    # baseline_piecewise(b, measure_type, s)
    # ridge_test(b, measure_type)
    pca_test(b, measure_type)
    # pca_test_raw(b, measure_type)
    return 0
    
main()
