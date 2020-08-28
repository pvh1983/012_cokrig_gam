import pandas as pd
import matplotlib.pyplot as plt


def label_point_orig(x, y, val, ax):
    a = pd.concat({'x': x, 'y': y, 'val': val}, axis=1)
    # print a
    for i, point in a.iterrows():
        ax.text(point['x'], point['y'], str(int(point['val'])))


if __name__ == "__main__":
    # read file
    ifile = 'output/results_run02_08262020/OK__fitted_variogram_WL2019_3yr_cutoff_200000.csv'
    df = pd.read_csv(ifile)
    df.head()

    fig, ax = plt.subplots()

    df.plot(ax=ax, x='dist', y='gamma', style='o', legend=False)
    label_point_orig(df.dist, df.gamma, df.np, ax)

    ax.set_xlabel('Distance (ft)')
    ax.set_ylabel('Semivariance')

    #title_out_stats = f'{sce}/{var} Peak Values by Zone'
    #ax.set_title(title_out_stats, fontsize=12)
    ofile = 'output/test.png'
    fig.savefig(ofile, dpi=300, transparent=False,
                bbox_inches='tight')

    plt.show()
