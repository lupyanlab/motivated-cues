#! /usr/bin/env python
# encoding: utf-8
import pandas as pd
import itertools as itls
import numpy as np

def extract_first_fixation(chunk):
    first_image = (chunk[['fixations_sound','fixations_label',
                          'fixations_both','fixations_neither']].sum(axis=1) == 1)
    if first_image.sum() > 0:
        chunk = chunk[first_image]
        return chunk[(chunk['fixations_total'] == chunk['fixations_total'].min())]
    else:
        return chunk[[False]*len(chunk)]

def sumDurationPerImage(chunk):
    summary = {x: chunk['eventDuration'][chunk['ordered_sound']==x].sum()
                      for x in ['a','b','c','d']}
    summary.update({x+'_soundTyp': chunk['eventImage_soundTyp'][chunk['ordered_sound']==x].mean()
                      for x in ['a','b','c','d']})
    return pd.DataFrame(summary, index=[chunk.index[0]])

def expand(df, bins=100):
    bins = int(bins)
    
    def round_up(x):
        """ Rounds a float to the next highest hundred """
        return (int(x+bins) - int(x%bins))

    def expand_fixations(chunk):
        """ Duplicates rows by reindexing """
        index = [[a]*chunk['iterations'][a] for a in chunk.index]
        index = pd.Index(itls.chain(*index), name='expandedIndex')
        return chunk.reindex(index)

    def add_time_bins(chunk):
        """ Determine the time bin for each duplicated event """
        tb = np.array(range(len(chunk)))
        tb = tb + int(chunk['eventStart_rounded'][0] / bins)
        chunk['time_bin'] = tb
        return chunk

    # get accurate time bins by rounding eventStart and eventEnd up
    df['eventStart_rounded'] = df['eventStart'].apply(round_up)
    df['eventEnd_rounded'] = df['eventEnd'].apply(round_up)
    
    # determine how many time bins each fixation falls into
    # that is, determine how many times each row needs to be copied
    df['iterations'] = (df['eventEnd_rounded']-df['eventStart_rounded'])/bins + 1
    
    # duplicate the rows
    df = df.groupby(['name','ROW_ID']).apply(expand_fixations)
    
    # determine the time bins for the rows
    df = df.reset_index(level='expandedIndex')
    df = df.groupby(['name','ROW_ID','expandedIndex']).apply(add_time_bins)
    df = df.reset_index(drop=True)
    return df

if __name__ == '__main__':
    raw = pd.read_csv('tys_results.csv')
    raw = raw[(raw['whichPart'] != 'practice')]
    
    ratings = pd.read_csv('imageratings_centered.csv')
    ratings['eventImageFile'] = ratings['picFile'] + '.jpg'
    ratings = ratings[['eventImageFile','zSound','mean_sound','ordered_sound']]
    
    raw = raw.merge(ratings, how='left', on='eventImageFile')
    
    raw['sinceImageOnset'] = raw['sinceImageOnset']*1000
    raw['sinceTargetOnset'] = raw['sinceTargetOnset']*1000
    raw['eventDuration'] = raw['eventDuration']*1000
    raw['eventStart'] = raw['sinceImageOnset'] - raw['eventDuration']
    raw['eventEnd'] = raw['sinceImageOnset']
    
    rts = raw[raw['eventDevice']=='mouse']
    #rts = rts.rename(columns={'sinceImageOnset':'rt'})
    rts = rts.rename(columns={'sinceTargetOnset':'rt'})
    rts = rts[['name','ROW_ID','category','cueType','sound_file','block',
               'trialIndex_new','rt','zSound','ordered_sound']]
    rts.to_csv('latencies.csv', na_rep='NA', index=False)
    
    fix = raw[raw['screenState']=='IMAGE_SCREEN']
    fix.to_csv('fixations.csv', index = False)
    
    first = fix.groupby(['name','ROW_ID','cueType']).apply(extract_first_fixation)
    first = first[['name', 'ROW_ID', 'category', 'cueType',
                   'sound_file', 'block', 'trialIndex_new', 'sinceImageOnset',
                   'eventStart', 'eventEnd', 'eventImageFile', 'eventImageLoc',
                   'zSound', 'ordered_sound']]
    first.to_csv('first_fixation.csv', na_rep='NA', index=False)
    
    dur = fix.groupby(['name','ROW_ID','cueType']).apply(sumDurationPerImage)
    dur = dur.reset_index()
    del dur['level_3']
    
    for ordered in ['a','b','c','d']:
        dur[ordered+'_weighted'] = dur[ordered] * dur[ordered+'_soundTyp']
    dur['weighted_zSound'] = dur[[x+'_weighted' for x in ['a','b','c','d']]].sum(axis = 1)
    dur['weighted_zSound'] = dur['weighted_zSound'] / dur[['a','b','c','d']].sum(axis = 1)
    
    dur.to_csv('durations.csv', na_rep='NA', index=False)
    
    tmc = expand(fix)
    tmc = tmc[((tmc['time_bin'] >= 0) & (tmc['time_bin'] <= 20))]
    tmc = tmc[['name', 'ROW_ID', 'category', 'cueType',
             'sound_file', 'block', 'trialIndex_new', 'sinceImageOnset',
             'eventStart', 'eventEnd', 'time_bin', 'eventDuration', 
             'eventImageLoc', 'eventImageType', 'eventImageFile', 'zSound',
             'ordered_sound']]
    tmc.to_csv('timecourse.csv', na_rep='NA', index=False)
    