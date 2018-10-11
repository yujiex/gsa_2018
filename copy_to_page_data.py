import pandas as pd
import glob
import shutil

df_to_plot = pd.read_csv("~/Dropbox/gsa_2017/input/FY/region_case_study_to_plot.csv")

# copy csv files from /page_data folder to corresponding folders fillbetween folders
# modify region_case_study_to_plot to change the building or case number of the plots
num = 2
case_building = df_to_plot[df_to_plot['caseNumber'] == num]['Building_Number'].tolist()
for (i, b) in enumerate(case_building):
    print(b)
    # files = glob.glob("/Users/yujiex/Dropbox/gsa_2017/page_data/case_study/{}*.csv".format(b))
    # for f in files:
    #     outfile = f.replace("case_study", "case_study_fillbetween_plot_{}/Region {}".format(num, i + 1))
    #     shutil.copyfile(f, outfile)
    files = glob.glob("/Users/yujiex/Dropbox/gsa_2017/plot_FY_weather/html/single_building/savings/{}*.png".format(b))
    for f in files:
        outfile = f.replace("plot_FY_weather/html/single_building/savings", "page_data/case_study_fillbetween_plot_{}/Region {}".format(num, i + 1))
        shutil.copyfile(f, outfile)
