# Dans Kata
Many songs are in different scales. Based on a given list of tunes the aim of this program is to provide an order, so that as less as possible strings need to be retuned.

# Installation
This project was created using [Stack](https://docs.haskellstack.org/en/stable/README/).

## Requirements
* Stack Version 2.7.3

# Usage

## Input File
A text file containing the tunes is required. For example, the content should look like the following:

```
DADF#AD
EADGHE
CGDGAD
D#BCGBD
DGDGAD
F#ADGHE
HGDGAD
DAC#EHE
CGCGAD
```

## Execution
```
usage: stack run -- [fixed | random]

arguments:
  fixed         returns the best order of the tunes starting with first tune
  random        returns the best order of the tunes based on all tunes
```
