spc1 = [54,12,6,48,96,90,138,132,180,174,222,216,264,258,306,300,348,342,390,384,60,18,102,144,186,228,270,312,354,396,66,24,108,150,192,234,276,318,360,402,
72,30,114,156,198,240,282,324,366,408,78,36,120,162,204,246,288,330,372,414,84,42,126,168,210,252,294,336,378,420,871,866,906,901,941,936,976,971,1011,1006,
1046,1041,1081,1076,1116,1111,1151,1146,1186,1181,1221,1216,1256,1251,1291,1286,1326,1321,1361,1356,876,911,946,981,1016,1051,1086,1121,1156,1191,1226,1261,1296,1331,1366,881,916,951,986,1021,
1056,1091,1126,1161,1196,1231,1266,1301,1336,1371,886,921,956,991,1026,1061,1096,1131,1166,1201,1236,1271,1306,1341,1376,891,926,961,996,1031,1066,1101,1136,1171,1206,1241,1276,1311,1346,1381,
896,931,966,1001,1036,1071,1106,1141,1176,1211,1246,1281,1316,1351,1386]

spc2 = [1392,1398,1440,1434,1482,1476,1524,1518,1566,1560,1608,1602,1650,1644,1692,1686,1734,1728,1776,1770,1404,1446,1488,1530,1572,1614,1656,1698,1740,1782,1410,1452,1494,1536,1578,1620,1662,1704,1746,1788,
1416,1458,1500,1542,1584,1626,1668,1710,1752,1794,1422,1464,1506,1548,1590,1632,1674,1716,1758,1800,1428,1470,1512,1554,1596,1638,1680,1722,1764,1806,2145,2140,2180,2175,2215,2210,2250,2245,2285,2280,
2320,2315,2355,2350,2390,2385,2425,2420,2460,2455,2495,2490,2530,2525,2565,2560,2600,2595,2635,2630,2150,2185,2220,2255,2290,2325,2360,2395,2430,2465,2500,2535,2570,2605,2640,2155,2190,2225,2260,2295,
2330,2365,2400,2435,2470,2505,2540,2575,2610,2645,2160,2195,2230,2265,2300,2335,2370,2405,2440,2475,2510,2545,2580,2615,2650,2165,2200,2235,2270,2305,2340,2375,2410,2445,2480,2515,2550,2585,2620,2655,
2170,2205,2240,2275,2310,2345,2380,2415,2450,2485,2520,2555,2590,2625,2660]

inlet = [[2661,2666,2667,2662],[2666,2671,2672,2667],[2676,2681,2682,2677],[2686,2691,2692,2687],[2696,2701,2702,2697],[2667,2672,2673,2668],[2677,2682,2683,2678],[2687,2692,2693,2688],[2697,2702,2703,2698],[2668,2673,2674,2669],[2678,2683,2684,2679],[2688,2693,2694,2689],[2698,2703,2704,2699],[2669,2674,2675,2670],[2679,2684,2685,2680],[2689,2694,2695,2690],[2699,2704,2705,2700],[2671,2676,2677,2672],[2681,2686,2687,2682],[2691,2696,2697,2692],
[2662,2667,2668,2663],[2672,2677,2678,2673],[2682,2687,2688,2683],[2692,2697,2698,2693],[2663,2668,2669,2664],[2673,2678,2679,2674],[2683,2688,2689,2684],[2693,2698,2699,2694],[2664,2669,2670,2665],[2674,2679,2680,2675],[2684,2689,2690,2685],[2694,2699,2700,2695]]

outlet = [[6306,6311,6312,6307],[6311,6316,6317,6312],[6321,6326,6327,6322],[6331,6336,6337,6332],[6341,6346,6347,6342],[6312,6317,6318,6313],[6322,6327,6328,6323],[6332,6337,6338,6333],[6342,6347,6348,6343],[6313,6318,6319,6314],[6323,6328,6329,6324],[6333,6338,6339,6334],[6343,6348,6349,6344],[6314,6319,6320,6315],[6324,6329,6330,6325],[6334,6339,6340,6335],[6344,6349,6350,6345],[6316,6321,6322,6317],[6326,6331,6332,6327],[6336,6341,6342,6337],
[6307,6312,6313,6308],[6317,6322,6323,6318],[6327,6332,6333,6328],[6337,6342,6343,6338],[6308,6313,6314,6309],[6318,6323,6324,6319],[6328,6333,6334,6329],[6338,6343,6344,6339],[6309,6314,6315,6310],[6319,6324,6325,6320],[6329,6334,6335,6330],[6339,6344,6345,6340]]

rogoset = [[421,442,446,424],[442,470,474,446],[474,502,506,478],[421,442,446,424],[442,470,474,446],[498,526,530,502],[530,558,562,534],[594,622,626,598],[658,686,690,662],[554,582,586,558],[610,638,642,614],[722,750,754,726],[754,782,786,758],[562,590,594,566],[626,654,658,630],[818,846,850,822],[642,670,674,646],[786,814,818,790],[436,462,466,439],[706,734,738,710],
[770,798,802,774],[424,446,450,427],[450,478,482,454],[674,702,706,678],[738,766,770,742],[514,542,546,518],[586,614,618,590],[482,510,514,486],[546,574,578,550],[650,678,682,654],[714,742,746,718],[666,694,698,670],[778,806,810,782],[810,838,842,814],[618,646,650,622],[682,710,714,686],[458,486,490,462],[698,726,730,702],[430,454,458,433],[490,518,522,494],
[762,790,794,766],[826,854,858,830],[730,758,762,734],[794,822,826,798],[526,554,558,530],[474,502,506,478],[506,534,538,510],[570,598,602,574],[558,586,590,562],[538,566,570,542],[602,630,634,606],[622,650,654,626],[686,714,718,690],[638,666,670,642],[750,778,782,754],[782,810,814,786],[590,618,622,594],[654,682,686,658],[433,458,462,436],[670,698,702,674],
[814,842,846,818],[462,490,494,466],[734,762,766,738],[798,826,830,802],[702,730,734,706],[766,794,798,770],[470,498,502,474],[582,610,614,586],[446,474,478,450],[478,506,510,482],[542,570,574,546],[614,642,646,618],[510,538,542,514],[574,602,606,578],[678,706,710,682],[742,770,774,746],[694,722,726,698],[806,834,838,810],[427,450,454,430],[646,674,678,650],
[710,738,742,714],[486,514,518,490],[726,754,758,730],[454,482,486,458],[518,546,550,522],[790,818,822,794],[758,786,790,762],[822,850,854,826],[502,530,534,506],[534,562,566,538],[598,626,630,602],[566,594,598,570],[630,658,662,634],[538,566,570,542],[602,630,634,606],[470,498,502,474],[498,526,530,502],[554,582,586,558],[666,694,698,670],[698,726,730,702],
[506,534,538,510],[570,598,602,574],[762,790,794,766],[826,854,858,830],[586,614,618,590],[730,758,762,734],[794,822,826,798],[650,678,682,654],[714,742,746,718],[778,806,810,782],[810,838,842,814],[618,646,650,622],[682,710,714,686],[458,486,490,462],[530,558,562,534],[430,454,458,433],[490,518,522,494],[594,622,626,598],[658,686,690,662],[610,638,642,614],
[722,750,754,726],[754,782,786,758],[562,590,594,566],[626,654,658,630],[818,846,850,822],[642,670,674,646],[786,814,818,790],[436,462,466,439],[706,734,738,710],[770,798,802,774],[424,446,450,427],[450,478,482,454],[674,702,706,678],[738,766,770,742],[514,542,546,518],[502,530,534,506],[482,510,514,486],[546,574,578,550],[566,594,598,570],[630,658,662,634],
[526,554,558,530],[582,610,614,586],[694,722,726,698],[726,754,758,730],[534,562,566,538],[598,626,630,602],[790,818,822,794],[614,642,646,618],[758,786,790,762],[822,850,854,826],[678,706,710,682],[742,770,774,746],[806,834,838,810],[427,450,454,430],[646,674,678,650],[710,738,742,714],[486,514,518,490],[558,586,590,562],[454,482,486,458],[518,546,550,522],
[622,650,654,626],[686,714,718,690],[638,666,670,642],[750,778,782,754],[782,810,814,786],[590,618,622,594],[654,682,686,658],[433,458,462,436],[670,698,702,674],[814,842,846,818],[462,490,494,466],[734,762,766,738],[798,826,830,802],[702,730,734,706],[766,794,798,770],[446,474,478,450],[478,506,510,482],[542,570,574,546],[510,538,542,514],[574,602,606,578]]

cur = [[794,766,762,790],[542,514,510,538],[798,770,766,794],[546,518,514,542],[802,774,770,798],[550,522,518,546],[810,782,778,806],[558,530,526,554],[814,786,782,810],[562,534,530,558],[818,790,786,814],[566,538,534,562],[822,794,790,818],[570,542,538,566],[826,798,794,822],[574,546,542,570],[830,802,798,826],[578,550,546,574],[838,810,806,834],[586,558,554,582],
[842,814,810,838],[590,562,558,586],[846,818,814,842],[594,566,562,590],[850,822,818,846],[598,570,566,594],[854,826,822,850],[602,574,570,598],[858,830,826,854],[606,578,574,602],[614,586,582,610],[618,590,586,614],[622,594,590,618],[626,598,594,622],[630,602,598,626],[634,606,602,630],[642,614,610,638],[646,618,614,642],[650,622,618,646],[654,626,622,650],
[658,630,626,654],[662,634,630,658],[670,642,638,666],[674,646,642,670],[678,650,646,674],[682,654,650,678],[686,658,654,682],[690,662,658,686],[698,670,666,694],[702,674,670,698],[706,678,674,702],[446,424,421,442],[710,682,678,706],[450,427,424,446],[454,430,427,450],[714,686,682,710],[458,433,430,454],[718,690,686,714],[462,436,433,458],[466,439,436,462],
[726,698,694,722],[474,446,442,470],[730,702,698,726],[478,450,446,474],[734,706,702,730],[482,454,450,478],[738,710,706,734],[486,458,454,482],[742,714,710,738],[490,462,458,486],[746,718,714,742],[494,466,462,490],[754,726,722,750],[502,474,470,498],[758,730,726,754],[506,478,474,502],[762,734,730,758],[510,482,478,506],[766,738,734,762],[514,486,482,510],
[770,742,738,766],[518,490,486,514],[774,746,742,770],[522,494,490,518],[782,754,750,778],[530,502,498,526],[786,758,754,782],[534,506,502,530],[790,762,758,786],[538,510,506,534]]