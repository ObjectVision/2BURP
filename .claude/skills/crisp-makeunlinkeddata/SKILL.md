---
name: crisp-makeunlinkeddata
description: Processing the MakeUnlinkedData buttons of the CRISP model one by one from the command line with GeoDmsRun (which engine, how the buttons resolve, order, logs, exit codes, durations, data prerequisites and their fallbacks, and the GeoDMS 17+ configuration rules that bite when the config has not run on a current engine for a while). Use when asked to (re)generate the unlinked data, run the model for a study area, or diagnose a failing button.
---

# Processing MakeUnlinkedData one button at a time

`cfg\main\MakeUnlinkedData.dms` defines the "unlinked data": files that the model reads
but that must be generated first. It is a table, `Buttons_List`, with 13 rows:

| # | label | writes | depends on |
|---|-------|--------|------------|
| 0 | a1_Countries | Regions/Countries.fss | - |
| 1 | a2_Continents | Regions/Continents.fss | a1 |
| 2 | b1_studyarea_def | data/StudyAreaTiles_<area>_54009.dbf (in the repo) | a2 |
| 3 | b2_Coastline_InlandWater | physical/Coast|Water/<area>/IsLandMass*, IsInlandWater* (1 km and 100 m) | b1 |
| 4 | b3_Hasland | physical/Water/<area>/HasLand_54009.tif | b2 |
| 5 | c1_RoundPastPop_and_Builtup | Results/<area>/Population_Y*, Builtup_total_Y* | b3 |
| 6 | c2_PastDoU | Results/<area>/DegUrbaGrid_Y1975..Y2020 | c1 |
| 7 | d1_Allocation | Results/<area>/*_Y2030..Y2100<FileSuffix> (allocation) | c2 |
| 8 | e1_reporting_countries | Regions/Countries_UN.fss | a2 |
| 9 | e2_reporting_regions | Regions/Intermediate_Regions_UN.fss | a2 |
| 10-12 | f1_pop_mozaik, f2_builtup_mozaik, f3_degurba_mozaik | Postprocessing mozaiks | d1 |

Paths are relative to `%SourceDataDir%\2BURP` (`ConfigSettings/Overridable/ToBURPDataDir`)
or `%LocalDataDir%\CRISP`. Each row has a `files_exist` check and a `gen_trigger` item.
The three containers `CreateFiles`, `RecreateFiles` and `Unavailable` are generated from
the table: a button appears under **CreateFiles** while its files do not exist yet, under
**RecreateFiles** once they do, and under **Unavailable** while the button it depends on
has not produced its files. So the item to run moves as you go; a run of an item that does
not exist ends with `... was not found` in the log and exit code 1.

## Engine and study area

- Run with the `.m` flavour of the newest installed engine,
  `C:\Program Files\ObjectVision\GeoDms<ver>.m\GeoDmsRun.exe` (20.19.2.m at the time of
  writing). Do not run from a copy of the dev build. `main.dms` requires 14.1 or newer.
- The study area comes from the `StudyArea` environment variable; without it
  `ModelParameters/StudyArea_manually` applies. `cfg\studyarea.txt` is not read by anything.
- `Use_TempTifFiles` can likewise be set through the environment.

## The one-by-one run

`batch\run_makeunlinkeddata.ps1` does exactly this, with one GeoDMS log per step:

```powershell
cd E:\prj\JRC\CRISP\batch
.\run_makeunlinkeddata.ps1 -StudyArea Europe
.\run_makeunlinkeddata.ps1 -StudyArea Africa -Steps c2_PastDoU, d1_Allocation
```

By hand, from `cfg\` (the log option must come first):

```
set StudyArea=Europe
"C:\Program Files\ObjectVision\GeoDms20.19.2.m\GeoDmsRun.exe" /Lc:\LocalData\CRISP\log\Europe_a1.log main.dms /MakeUnlinkedData/CreateFiles/a1_Countries
```

Then, per step, before starting the next one:

1. Exit code 0 and no `[E]`/`[F]` lines in the log. `grep -c "\[E\]" <log>` is enough;
   `[W]` lines about unknown escape codes in `Physical.dms` and about the missing
   `%env:StudyArea%` placeholder are expected.
2. The `Writing to ...` lines of the log name the files written; check their timestamps.
3. Run the steps in table order. A later step reads the files of the earlier ones through
   read-only items, so a stale or missing file shows up as a read error, not as a recompute.

If you drive GeoDmsRun from Git Bash, set `MSYS_NO_PATHCONV=1` first: otherwise
`/MakeUnlinkedData/...` is rewritten into a Git installation path before GeoDMS sees it.

Durations seen on OVSRV10 (32 cores, 128 GB) with GeoDMS 20.19.2, 1 km Mollweide:

| step | Europe | Africa |
|------|--------|--------|
| a1, a2 | 8 s each | 8 s, 6 s |
| b1_studyarea_def | 1 s | - |
| b2_Coastline_InlandWater | 77 s | 111 s |
| b3_Hasland | 9 s | 38 s |
| c1_RoundPastPop_and_Builtup | 13 s | 19 s |
| c2_PastDoU | 3 min | 8 min |
| d1_Allocation | 9 min | 27 min (peak about 20 GB) |

The Africa figures are from the 2024 configuration; its indicator exports took another 31 min.

## Data prerequisites and the fallbacks in ModelParameters

`main` refers to three datasets that are not on the share (CRISP issue #132):
`Regions/fas_combined_20250428_54009.*`, `Population/POP_<year>_1000_WPP2024_v2.tif` and
`Regions/UNBNDA_CTY_onLAND_54009_1000.tif` with its attributes csv. Until they arrive,
these parameters select what is available locally; flip them back when the files are on
the share:

| parameter | now | with the main data |
|-----------|-----|--------------------|
| FunctionalAreas_filedate | '20240627' | '20250428' |
| FunctionalAreas_gridval_from_order | derived: true for 20240627 (no gridval field, raster ids are record order + 1) | false |
| Claim_fileversion | 'v8' (1005 areas, matches 20240627) | 'v12' (983 areas, matches 20250428) |
| Population_fileversion | '' | '_v2' |
| Use_UN_Country_Grid | FALSE (domain not restricted to UN territories) | TRUE |
| Additional_FileSuffix | '_pop_wpp24_fa20240627_calib_20241205' | '_pop_wpp24_v2_calib_20241205' |

The e1/e2 reporting buttons need the UN grid and its attributes csv and fail without them
(`Countries_UN_in/iso3cd` unknown, `Countries_UN.fss` missing); the model then reports on
`World` instead of UN countries and regions. The f1..f3 mozaik buttons only become
available when the d1 results of all six continents exist with the same FileSuffix,
because a mozaik merges the per-continent grids into a world grid.

Outcome of the Europe run of 2026-09-04 with 20.19.2 and the fallbacks above: a1..d1 all
exit code 0 without error lines; e1/e2 blocked by the UN data; f1..f3 unavailable until
the other continents are run.

## GeoDMS 17+ rules the configuration has to follow

The configuration was written for GeoDMS 15/16 and is adapted; keep new code to these rules
or the first button that touches it fails at meta-info time:

- A calculated item that writes a `.tif` needs `StorageType = "tif"` (or
  `gdalwrite.grid`, which cannot write bool grids). Without it the `.tif` defaults to the
  read-only `gdal.grid` manager: "Item has both a Calculation Rule and a read-only storage
  spec". Template writers use `for_each_nedvat(name, expr, domain, values, storage, 'tif')`.
- Literal value arrays must have exactly as many values as their domain. The
  `AllShares/Moving_up_probability_data_*` arrays are padded with zeros to 101 values.
- `point(a, b, unit)` no longer exists; it meant `point_yx(a, b, unit)` (a is the row).
- To find such problems without running the model:
  `GeoDmsRun.exe /L<log> main.dms @sourcedescr <a shallow writer item>` and grep the log
  for `[E]`. Do not use `@sourcedescr` on deep items such as the allocation results: it
  needs unit cardinalities and computes, and writes, most of the model.
- Running the same probe against older installed engines
  (`C:\Program Files\ObjectVision\GeoDms*`) bisects the version that introduced a change.

## Where things end up

- `F:\SourceData\2BURP\Regions\` and `...\physical\Coast|Water\<area>\`: a1 to b3.
- `c:\LocalData\CRISP\Results\<area>\`: c1, c2, d1 grids; `Temp\<area>\`: suitability grids.
- `c:\LocalData\CRISP\Indicators\<area>\`: csv exports; `data\` in the repo: b1's tile dbf.
- Every written file gets a sidecar `.xml` naming the engine version, config item and
  StudyArea that produced it: the fastest way to see what a file on disk came from.
