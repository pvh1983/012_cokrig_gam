import pandas as pd
import matplotlib.pyplot as plt


def label_point_orig(x, y, val, ax):
    a = pd.concat({'x': x, 'y': y, 'val': val}, axis=1)
    # print a
    for i, point in a.iterrows():
        ax.text(point['x'], point['y'], str(int(point['val'])))


if __name__ == "__main__":

    opt_gen_variograms = False
    opt_pre_process_data = True

    # Read dataframe and filter out nan cells

    if opt_pre_process_data:
        ifile = 'input/data/water_levels/Shallow.Wells.GMA12.YJ.hds.Smooth.csv'
        year = 1929
        Mea_WL = 'WL2020'
        aquifer_type = 'Shallow.Below'
        list_ras_res = [500, 4000, 8000, 16000, 32000, 64000]
        df0 = pd.read_csv(ifile)
        c1 = [Mea_WL]
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

        # hist plot of drawdown
        ptype = 'kde'  # 'hist', 'kde'
        fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(8, 8), sharey=False)

        c3 = [
            f'{Mea_WL}.minus.{aquifer_type}.{year}.{ras_res}' for ras_res in list_ras_res]
        df_sim_dd = df[c3]
        if ptype == 'kde':
            df_sim_dd.plot.kde(ax=ax, alpha=0.9)  # type1
        elif ptype == 'hist':
            df_sim_dd.plot.hist(ax=ax, bins=30, alpha=0.5)  # type 2
        #ax.set_xlabel('Simulation Time in Years')
        #ax.set_ylabel(var + ' Concentration, pCi/L')

        title_out_stats = f'Simulated 1929-2020 drawdown (ft)'
        ax.set_title(title_out_stats, fontsize=12)
        ax.legend()
        # ax.set_ylim(ylim)
        ofile_png = f'output/simulated_drawdown_1929_2020_{ptype}.png'
        fig.savefig(ofile_png, dpi=300, transparent=False,
                    bbox_inches='tight')
        print(f'The figure was saved at: {ofile_png}')
        # plt.show()

        # Print data to file
        ofile = f'{ifile[:-4]}.hp.csv'
        df.to_csv(ofile)

    if opt_gen_variograms:
        # read file
        list_res = [500, 32000]
        ptitle = 'Shallow.Combined.Hds.2020'
        fig, ax = plt.subplots()
        for i, res in enumerate(list_res):
            ifile = f'output/fitted_variogram_WL2020_3yr_cutoff_200000_GAM_res_{res}.csv'
            df = pd.read_csv(ifile)
            df.head()

            df.plot(ax=ax, x='dist', y='gamma',
                    style='-', legend=True, label=f'GAM_res_{res}ft')
            label_point_orig(df.dist, df.gamma, df.np, ax)

            ax.set_title('Distance (ft)')
            ax.set_xlabel('Distance (ft)')
            ax.set_ylabel('Semivariance')

        # title_out_stats = f'{sce}/{var} Peak Values by Zone'
        # ax.set_title(title_out_stats, fontsize=12)
        ofile = f'output/variogram_{ptitle}.png'
        fig.savefig(ofile, dpi=300, transparent=False,
                    bbox_inches='tight')

        plt.show()

# ref
# https://realpython.com/python-histograms/
