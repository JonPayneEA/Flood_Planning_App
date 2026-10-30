## Status of this page

These are working notes on where the economics figures come from. Nobody on the project is an economist, so every value here is a planning value with its source stated, not an agreed figure. The values in use are in the table above. This page records why they are what they are, and what is still assumed. Last updated 30 October 2026.

## The method in one paragraph

The expected annual damage for an area is the sum over the four rivers and sea flood bands of: households in the band, times the band's annual chance of flooding, times the damage to one household in one flood event, times a price uplift. It is worked in three scenarios (low, central and high) that move together. Only the damage figure and the price uplift are discussed on this page. The band probabilities are explained on the economics card in the Guide.

## Evidence for the damage per household

Each figure is for structure and contents damage to a home in one flood event. Temporary accommodation, emergency costs and health values are separate categories and are not included.

| Figure | Source | Where | Price year | What it includes |
|---|---|---|---|---|
| £23,000 | The costs and impacts of the winter 2013 to 2014 floods, Environment Agency report | Pages 47 to 48, Table 5.3 and section 5.2.2 | Not stated. The event is winter 2013 to 2014. | Economic damage per flooded residential property, derived from insured fluvial and groundwater claims, then extended to coastal flooding and uninsured homes |
| £52,000 | Same report | Pages 43 to 44, Table 5.1 | Not stated. Same event. | The severe, prolonged Somerset value, the top of a range of £6,000 to £52,000 |
| About £30,000 | Environment Agency key lines document, "Flood Action Campaign" section | Page 13 | Not stated. The document is labelled Technical 2024. | "The average cost of flooding to a home is around £30,000." Original source not confirmed |
| £23,000 to £30,000 | The costs of the summer 2007 floods in England, project SC070039 | Page 10, section 3.2 | The range gives none. The report's total economic costs are in 2007 prices. | The average spent per domestic property, a financial claims-based value, not an economic damage value |

Checks the research raised:

- The winter 2013 to 2014 figure of £23,000 does not divide cleanly from the report's own totals. £320 million over 10,465 properties is about £30,600. The report still names £23,000 as its best estimate per asset, and the £30,600 sits close to the key-line figure.
- The 2007 flooded-homes count differs between files: 46,000 to 48,000 in the detailed report and 55,000 in a key-lines table that appears to include businesses.
- No complete Multi-Coloured Handbook depth-damage table was found. The one explicit point is £16,500 of building-fabric damage at 0.6 m for flooding shorter than 12 hours.
- No winter 2015 to 2016 residential-only damage total was found. Dividing the whole-event total by homes would mix in businesses, transport and other costs, so it was not done.

## Values chosen

- Low £23,000, central £30,000 and high £52,000 per household per event.
- The values are stored as the sources state them. Prices are brought to a common year by the price uplift, not by editing the figures.

## Bringing the figures to current prices

The guidance for flood and coastal erosion risk management projects (LIT 66371, Allowing for inflation in FCERM projects) says to use the GDP deflator for benefits, by calendar year, and to use the latest version published by the Office for National Statistics. Its Table A1 is a June 2025 example and starts at 2019, so it has no value for 2007, 2013 or 2014.

| Calendar year | Index (2024 = 100) | Source |
|---|---|---|
| 2007 | 64.7322 | GDP deflators workbook, March 2026 |
| 2013 | 73.4795 | GDP deflators workbook, March 2026 |
| 2014 | 74.6524 | GDP deflators workbook, March 2026 |
| 2025 | 103.180 | Guidance Table A1, June 2025 |
| 2026 | 104.903 | Guidance Table A1, June 2025 |

The factor from 2014 to 2026 is 104.903 divided by 74.6524, which is 1.405. From 2013 it is 1.428. The two differ by 1.6 per cent. The choice of 2014 gives the smaller factor.

Two cautions apply. The historical years and the target year come from different releases, and historical values can be revised. The 2026 value is a forecast. The 2029 value in Table A1 was not used, because a forecast three years out is not a current price.

## Decisions and what is assumed

- **Price year of the damage figures:** taken as 2014. The sources do not state it. This is an assumption.
- **One factor for all three scenarios:** the three figures do not have a demonstrated common price year. One factor is used so the scenarios stay in order, on the reading that the £30,000 key line is of the same order and era as the 2013 to 2014 figures. If it is in recent prices, the central figure is about 40 per cent too high.
- **Target price year:** 2026.
- **Uplift value:** 1.405, recorded in the assumptions table with its source and price year.

## What would settle the open questions

- The price base stated in the 2013 to 2014 report. A reading by someone with access to its methodology annex may settle it.
- The original source of the key-line £30,000.
- One release of the GDP deflator series that holds 2014 and the target year together. Recalculate the factor from it.
- Whether the model should include the other cost categories, which would be a change of method, not of figure.

## Prompts for the files assistant

First prompt, to find the damage figures:

```
I need figures for a flood exposure model, taken only from the files you can access. Do not use general knowledge or estimate. If a figure is not in the files, say so.

Context
We estimate expected annual damage to homes in Flood Warning Areas in England. For each NaFRA2 flood band the model multiplies households by the annual chance of flooding by the damage to one household in one flood event. We need that damage figure in three scenarios (low, central, high), plus a way to bring it to current prices.

Find, with the file name and the page or table for every figure:

1. Average damage per residential property per flood event, in pounds. Preferred sources: the Multi-Coloured Handbook (state which edition), Environment Agency or Defra appraisal guidance, FCERM appraisal supplementary notes, and EA event cost reports (summer 2007, winter 2013 to 2014, winter 2015 to 2016). Give the figure by flood depth or by standard of protection if it is split that way.
2. Whether each figure is per event or an annual average damage. Say which. Do not convert one into the other.
3. Whether each figure includes the property structure, contents, emergency costs, and any intangible or health costs. List what it includes.
4. The price year of each figure (the year the pounds are in) and the discount or inflation basis.
5. Any guidance on bringing older figures to current prices: the index named (for example the GDP deflator, a construction tender price index), the factor or table, and the years it covers.
6. Any number for households flooded in a named event, with the matching residential damage total, so an average can be derived. Show the division.

Output
A table with the columns: figure, value in pounds, per event or annual average, what it includes, price year, source file, page or table, quote.
Then three short parts:
- A recommended low, central and high damage per household per flood event, using only figures from the files, with the reasoning in two or three sentences.
- A recommended price uplift factor and the year it brings the figures to, if the files support one. If they do not, say so.
- A list of anything uncertain, conflicting between files, or missing.

Rules
- Quote the source text exactly where a number comes from it.
- Where files disagree, show both and do not choose silently.
- UK English. Plain prose. No em dashes.
```

Second prompt, to find the price year and the deflator values:

```
I need the price year of three damage figures and the GDP deflator values to bring them to current prices, taken only from the files you can access. Do not use general knowledge or estimate. If a value is not in the files, say so.

Context
A flood model uses three damage figures per household per flood event: 23,000, 30,000 and 52,000 pounds. They come from the winter 2013 to 2014 floods report (the 23,000 and 52,000, with 52,000 the severe, prolonged Somerset value), from an Environment Agency key line (about 30,000), and from the Association of British Insurers range for summer 2007 (23,000 to 30,000). I need to bring them to current prices with the GDP deflator.

Find, with the file name and the page or table for every item:

1. The price year or price base stated for each of the three figures in its own source. Quote the sentence. If none is stated, say "not stated" and give the nearest year the text refers to, without choosing for me.
2. The GDP deflator series in the inflation guidance for flood and coastal erosion risk management projects (for benefits): the full table of index values by year, with the base year of the index and whether the years are financial or calendar years.
3. The index value for each of these years: 2007, 2013, 2014, and the most recent year in the table. If a year is missing, say so.
4. Any instruction in the guidance about which year to use as the base for benefits, and about using the latest ONS or Treasury version in place of the table in the file. Quote it.
5. Any factor the guidance gives directly for bringing an older year to a recent year. Quote it and give the years it covers.

Output
A table with the columns: item, value, year basis (financial or calendar), source file, page or table, quote.
Then:
- For each of the three damage figures, the uplift factor to the latest year in the table, worked as the latest index divided by the index for the figure's price year. Show the division and the figures used. If the price year is not stated, give the factor for each candidate year, labelled as such.
- A list of anything uncertain, conflicting between files, or missing.

Rules
- Quote the source text exactly where a number comes from it.
- Where files disagree, show both and do not choose silently.
- Do not apply one uplift factor to all three figures unless they share a price year. Say so if they do not.
- UK English. Plain prose. No em dashes.
```
