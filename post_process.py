import pandas as pd
import matplotlib.pyplot as plt

# Notes: Choose a "year" for raster WL file
# cd C:/Users/hpham/OneDrive - INTERA Inc/projects/012_rscript_geostats_s/scripts


def label_point_orig(x, y, val, ax):
    a = pd.concat({'x': x, 'y': y, 'val': val}, axis=1)
    # print a
    for i, point in a.iterrows():
        ax.text(point['x'], point['y'], str(int(point['val'])))


if __name__ == "__main__":

    # Read Ross's WL csv file, choose some cols, and get DIFF
    # This generates the input for the R scripts
    opt_pre_process_data = True

    opt_gen_variograms = True
    opt_gen_variograms_aio = True

    year = 2019  # year of GAM ras WL
    Mea_WL = 'WL2019'
    aquifer_type = 'Shallow.Below'
    list_ras_res = [500, 4000, 8000, 16000, 32000, 64000, 128000]
    method = 'OK_Water_Levels'

    # [1] Read dataframe and filter out nan cells
    if opt_pre_process_data:
        ifile = 'input/data/water_levels/Shallow.Wells.GMA12.YJ.hds.Smooth.csv'
        print(f'Reading {ifile}\n')
        df0 = pd.read_csv(ifile)
        c1 = ['Longitude', 'Latitude', Mea_WL]
        c2 = [
            f'{aquifer_type}.{year}.{ras_res}' for ras_res in list_ras_res]
        list_cols = c1 + c2
        df = df0[list_cols]
        df = df[df[Mea_WL] > 0]  #
        print(f'Warning: Only choose cells that have measured water levels > 0.')
        df.head()
        n_mea = df.shape[0]

        for ras_res in list_ras_res:
            new_col_name = f'{Mea_WL}.minus.{aquifer_type}.{year}.{ras_res}'
            SS_WL_col_name = f'{aquifer_type}.{year}.{ras_res}'
            df[new_col_name] = df[Mea_WL] - df[SS_WL_col_name]

        # [1.2] hist plot of drawdown
        list_ptype = ['hist', 'kde']  # 'hist', 'kde'
        c3 = [
            f'{Mea_WL}.minus.{aquifer_type}.{year}.{ras_res}' for ras_res in list_ras_res]
        fig, ax = plt.subplots(nrows=1, ncols=2, figsize=(
            22, 6), sharex=False, sharey=False)
        #fig.delaxes(ax[1, 3])
        for i, ax in enumerate(fig.axes):
            # for ptype in list_ptype:
            ptype = list_ptype[i]

            df_sim_dd = df[c3]
            if ptype == 'kde':
                df_sim_dd.plot.kde(ax=ax, alpha=0.9)  # type1
            elif ptype == 'hist':
                df_sim_dd.plot.hist(ax=ax, bins=30, alpha=0.5)  # type 2
            #ax.set_xlabel('Simulation Time in Years')
            #ax.set_ylabel(var + ' Concentration, pCi/L')

            #title_out_stats = f'1929-2020 residual (ft)'
            #ax.set_title(title_out_stats, fontsize=12)
            ax.legend()
            # ax.set_ylim(ylim)

        #
        ofile_png = f'output/residual_{year}_{Mea_WL}.png'
        fig.savefig(ofile_png, dpi=300, transparent=False,
                    bbox_inches='tight')
        print(f'The figure was saved at: {ofile_png}')
        # plt.show()

        # [1.4] Print data to file
        ofile = f'{ifile[:-4]}_{year}_hp.csv'
        df.to_csv(ofile, index=False)

    if opt_gen_variograms:
        # read file
        #list_res = [500, 32000]
        ptitle = f'Empirical semivariograms'
        fig, ax = plt.subplots(nrows=3, ncols=2, figsize=(
            15, 9), sharex=True, sharey=True)
        #fig.delaxes(ax[1, 3])
        for i, ax in enumerate(fig.axes):
            #fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(8, 8), sharey=False)
            # for i, ras_res in enumerate(list_ras_res):
            ras_res = list_ras_res[i]
            ifile2 = f'output/{method}_fitted_variogram_{Mea_WL}.minus.{aquifer_type}.{year}.{ras_res}.csv'
            df = pd.read_csv(ifile2)
            df.head()

            df.plot(ax=ax, x='dist', y='gamma',
                    style=':o', legend=True, label=f'GAM_res_{ras_res}ft', fontsize=8)
            label_point_orig(df.dist, df.gamma, df.np, ax)

            ax.set_title(ptitle)
            ax.set_xlabel('Distance (ft)')
            ax.set_ylabel('Semivariance')

        # title_out_stats = f'{sce}/{var} Peak Values by Zone'
        # ax.set_title(title_out_stats, fontsize=12)
        ofile = f'output/variogram_{year}_{Mea_WL}.png'
        fig.savefig(ofile, dpi=300, transparent=False,
                    bbox_inches='tight')
        print(f'Saved a figure at {ofile}\n')
        # plt.show()

    if opt_gen_variograms_aio:
        # read file
        #list_res = [500, 32000]
        ptitle = 'Empirical semivariograms'
        fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(
            9, 9), sharex=True, sharey=True)
        plt.grid(color='#e6e6e6', linestyle='-', linewidth=0.5, axis='both')
        #fig.delaxes(ax[1, 3])
        # for i, ax in enumerate(fig.axes):
        #fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(8, 8), sharey=False)
        for i, ras_res in enumerate(list_ras_res):
            #    ras_res = list_ras_res[i]
            ifile2 = f'output/{method}_fitted_variogram_{Mea_WL}.minus.{aquifer_type}.{year}.{ras_res}.csv'
            print(f'Reading {ifile2} \n')
            df = pd.read_csv(ifile2)
            df.head()

            df.plot(ax=ax, x='dist', y='gamma',
                    style='-', legend=True, label=f'GAM_res_{ras_res}ft')
            #label_point_orig(df.dist, df.gamma, df.np, ax)

            ax.set_title(ptitle)
            ax.set_xlabel('Distance (ft)')
            ax.set_ylabel('Semivariance')

        # title_out_stats = f'{sce}/{var} Peak Values by Zone'
        # ax.set_title(title_out_stats, fontsize=12)
        ofile = f'output/variogram_{year}_{Mea_WL}_aio.png'
        fig.savefig(ofile, dpi=300, transparent=False,
                    bbox_inches='tight')
        print(f'Saved a figure at {ofile}\n')
        # plt.show()

# ref
# https://realpython.com/python-histograms/
