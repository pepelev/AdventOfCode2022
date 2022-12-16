module Task4 where

import Data.List (splitAt, findIndex)

input = "61-78,61-77\n\
\1-98,23-98\n\
\70-70,3-70\n\
\7-41,7-40\n\
\5-85,2-50\n\
\13-59,13-13\n\
\22-81,57-82\n\
\4-26,27-70\n\
\37-86,38-86\n\
\3-49,48-48\n\
\43-52,42-52\n\
\6-95,6-94\n\
\27-66,26-65\n\
\15-16,16-89\n\
\29-61,22-86\n\
\9-82,10-83\n\
\2-40,39-42\n\
\5-67,6-68\n\
\6-57,13-87\n\
\70-92,69-71\n\
\37-53,50-59\n\
\2-94,1-94\n\
\11-81,10-82\n\
\3-83,1-3\n\
\78-90,77-93\n\
\10-93,10-11\n\
\14-81,32-80\n\
\6-29,5-47\n\
\91-99,19-91\n\
\6-87,6-86\n\
\29-94,93-94\n\
\3-18,17-18\n\
\44-44,45-91\n\
\25-25,26-54\n\
\30-83,30-30\n\
\50-86,27-87\n\
\8-40,9-40\n\
\25-78,24-79\n\
\3-12,11-95\n\
\47-53,47-52\n\
\27-47,27-47\n\
\19-67,19-20\n\
\98-98,1-99\n\
\9-38,8-90\n\
\6-98,7-11\n\
\8-59,50-53\n\
\37-99,36-99\n\
\1-98,1-99\n\
\14-14,15-44\n\
\13-95,4-95\n\
\1-99,2-98\n\
\43-80,44-79\n\
\14-87,5-88\n\
\21-57,38-56\n\
\41-49,9-50\n\
\30-67,29-66\n\
\64-94,83-93\n\
\18-93,2-13\n\
\56-61,3-62\n\
\99-99,42-90\n\
\21-83,22-71\n\
\20-86,12-41\n\
\12-77,81-89\n\
\61-97,60-61\n\
\94-95,10-95\n\
\7-98,7-99\n\
\57-80,3-99\n\
\15-88,16-57\n\
\46-64,21-65\n\
\59-78,59-99\n\
\3-86,24-36\n\
\3-45,2-63\n\
\65-66,65-69\n\
\11-63,41-64\n\
\3-76,8-75\n\
\26-50,49-86\n\
\7-98,8-98\n\
\44-88,43-87\n\
\10-25,9-99\n\
\35-83,34-83\n\
\14-41,14-40\n\
\26-35,29-36\n\
\21-95,22-22\n\
\3-84,2-83\n\
\81-90,2-89\n\
\11-50,11-11\n\
\44-52,45-51\n\
\10-67,10-68\n\
\88-99,9-24\n\
\20-85,6-86\n\
\78-93,77-94\n\
\13-14,13-35\n\
\65-80,57-79\n\
\47-98,46-48\n\
\65-93,64-92\n\
\36-79,37-78\n\
\38-38,37-93\n\
\25-83,24-83\n\
\8-91,90-91\n\
\15-85,15-84\n\
\72-87,42-72\n\
\13-83,11-84\n\
\12-92,11-41\n\
\4-86,74-84\n\
\79-92,82-93\n\
\37-89,88-88\n\
\12-83,82-82\n\
\64-83,63-82\n\
\10-12,11-76\n\
\5-70,5-69\n\
\24-66,23-66\n\
\51-63,27-63\n\
\6-84,5-84\n\
\68-85,77-86\n\
\45-69,69-92\n\
\28-90,17-91\n\
\30-45,30-46\n\
\39-40,40-86\n\
\39-74,74-75\n\
\29-98,14-28\n\
\4-40,1-40\n\
\15-90,15-97\n\
\6-19,5-42\n\
\2-72,1-73\n\
\27-81,81-82\n\
\4-91,3-90\n\
\33-95,33-96\n\
\24-90,23-23\n\
\88-88,15-89\n\
\77-78,70-78\n\
\28-80,11-27\n\
\36-91,35-93\n\
\98-99,3-98\n\
\13-70,63-70\n\
\3-73,49-73\n\
\66-87,65-86\n\
\12-93,13-14\n\
\76-96,89-96\n\
\24-72,25-71\n\
\68-71,67-73\n\
\1-6,5-89\n\
\19-88,19-96\n\
\5-5,5-21\n\
\19-20,20-50\n\
\76-97,75-97\n\
\7-98,18-97\n\
\24-25,25-50\n\
\4-94,3-94\n\
\66-92,65-92\n\
\21-29,21-29\n\
\5-43,6-43\n\
\19-94,19-95\n\
\17-60,27-59\n\
\10-75,11-74\n\
\57-84,57-85\n\
\6-68,67-69\n\
\66-66,22-65\n\
\62-63,61-63\n\
\17-86,17-87\n\
\10-81,82-86\n\
\12-90,12-89\n\
\32-98,31-99\n\
\83-85,10-84\n\
\19-90,2-18\n\
\83-85,14-84\n\
\50-69,68-68\n\
\25-96,25-95\n\
\77-90,80-91\n\
\68-90,73-89\n\
\17-89,14-88\n\
\45-55,46-55\n\
\28-98,98-98\n\
\54-78,54-54\n\
\28-34,28-34\n\
\69-76,70-70\n\
\13-54,12-54\n\
\28-46,16-45\n\
\20-92,18-92\n\
\64-65,11-65\n\
\49-67,11-49\n\
\16-17,17-98\n\
\19-67,8-66\n\
\19-27,9-64\n\
\52-59,29-59\n\
\43-78,4-77\n\
\62-62,63-99\n\
\5-6,6-71\n\
\5-91,4-92\n\
\3-9,8-69\n\
\52-53,53-53\n\
\77-80,78-80\n\
\26-97,97-99\n\
\8-61,35-60\n\
\22-92,6-91\n\
\8-68,7-9\n\
\3-6,5-83\n\
\1-1,3-80\n\
\4-91,90-92\n\
\18-73,72-72\n\
\12-19,13-18\n\
\21-73,20-73\n\
\69-92,70-70\n\
\9-75,10-10\n\
\78-80,79-80\n\
\52-54,18-53\n\
\95-98,30-95\n\
\3-91,3-91\n\
\3-95,2-95\n\
\9-81,9-9\n\
\13-88,14-89\n\
\24-67,25-66\n\
\23-97,23-98\n\
\30-30,1-31\n\
\20-61,57-61\n\
\38-38,38-98\n\
\2-95,1-96\n\
\11-92,10-83\n\
\22-92,8-27\n\
\45-97,27-96\n\
\25-98,1-99\n\
\10-79,62-80\n\
\2-25,1-90\n\
\12-92,25-93\n\
\1-99,98-98\n\
\67-90,67-67\n\
\15-95,2-96\n\
\16-64,16-63\n\
\4-18,17-89\n\
\7-87,45-82\n\
\2-3,3-95\n\
\72-75,64-77\n\
\3-5,6-56\n\
\16-96,16-90\n\
\76-84,75-83\n\
\27-27,28-52\n\
\24-39,38-84\n\
\16-97,12-97\n\
\11-98,10-99\n\
\3-13,2-94\n\
\26-40,25-41\n\
\44-74,59-73\n\
\66-75,53-74\n\
\1-96,2-96\n\
\1-85,1-85\n\
\9-12,11-61\n\
\45-82,46-82\n\
\14-98,7-97\n\
\65-65,19-65\n\
\1-36,7-35\n\
\20-49,19-50\n\
\7-99,4-6\n\
\20-85,21-86\n\
\46-99,2-99\n\
\6-6,7-89\n\
\89-89,62-90\n\
\7-93,3-92\n\
\26-68,27-27\n\
\30-61,29-61\n\
\51-75,74-74\n\
\7-76,19-75\n\
\12-13,12-82\n\
\22-55,23-54\n\
\1-53,54-63\n\
\20-87,20-86\n\
\86-86,87-87\n\
\46-72,46-47\n\
\54-65,23-33\n\
\8-93,6-6\n\
\2-96,1-96\n\
\87-99,86-98\n\
\30-71,31-71\n\
\3-94,93-93\n\
\7-96,6-6\n\
\21-74,20-75\n\
\10-90,11-90\n\
\19-90,18-19\n\
\69-69,26-70\n\
\59-97,59-98\n\
\6-68,3-7\n\
\2-17,16-65\n\
\2-98,3-98\n\
\71-95,31-71\n\
\37-52,36-52\n\
\72-87,73-87\n\
\29-82,46-79\n\
\65-77,14-78\n\
\7-92,6-85\n\
\8-35,15-36\n\
\7-59,1-59\n\
\93-95,6-93\n\
\47-93,48-48\n\
\5-24,1-23\n\
\1-81,10-80\n\
\32-65,1-66\n\
\64-90,65-66\n\
\21-65,7-20\n\
\11-85,12-86\n\
\89-89,73-90\n\
\62-92,91-93\n\
\48-60,2-59\n\
\13-44,13-23\n\
\11-32,30-33\n\
\17-96,18-96\n\
\19-91,14-14\n\
\25-54,24-54\n\
\7-92,9-93\n\
\34-91,34-99\n\
\80-89,71-88\n\
\37-58,57-57\n\
\27-99,5-99\n\
\3-89,1-4\n\
\34-90,91-95\n\
\10-27,11-26\n\
\37-58,37-59\n\
\57-81,56-82\n\
\9-73,66-74\n\
\5-78,42-78\n\
\30-77,26-92\n\
\41-61,41-41\n\
\65-74,65-74\n\
\8-86,7-86\n\
\1-94,2-93\n\
\26-65,63-65\n\
\4-95,4-71\n\
\54-71,55-58\n\
\9-49,11-48\n\
\33-42,39-41\n\
\55-69,56-68\n\
\28-89,28-46\n\
\7-91,91-94\n\
\77-98,97-97\n\
\19-37,15-37\n\
\94-94,90-95\n\
\3-63,2-97\n\
\28-84,28-85\n\
\18-20,19-44\n\
\53-61,43-61\n\
\76-98,76-77\n\
\96-98,10-97\n\
\33-89,17-89\n\
\22-99,21-22\n\
\17-28,23-28\n\
\9-9,10-16\n\
\55-57,10-56\n\
\6-55,6-6\n\
\10-82,5-81\n\
\21-22,21-48\n\
\16-98,4-15\n\
\41-72,42-59\n\
\32-49,46-49\n\
\38-87,38-95\n\
\2-2,2-29\n\
\38-78,38-79\n\
\19-25,17-25\n\
\99-99,13-99\n\
\1-2,3-80\n\
\60-93,59-94\n\
\35-84,34-84\n\
\44-98,44-98\n\
\16-32,70-78\n\
\33-39,15-32\n\
\13-86,13-85\n\
\1-5,6-42\n\
\38-48,38-38\n\
\37-39,38-98\n\
\94-94,10-95\n\
\54-58,54-59\n\
\20-68,21-68\n\
\21-63,20-64\n\
\73-74,8-73\n\
\68-97,69-76\n\
\54-89,2-90\n\
\3-94,4-95\n\
\32-94,29-55\n\
\75-75,19-75\n\
\7-67,8-68\n\
\5-44,6-48\n\
\84-84,5-84\n\
\2-89,2-98\n\
\22-45,21-45\n\
\9-94,8-8\n\
\13-87,20-87\n\
\18-19,18-70\n\
\18-46,17-47\n\
\56-82,57-57\n\
\17-77,28-78\n\
\62-87,61-88\n\
\8-87,7-88\n\
\10-90,9-91\n\
\4-92,4-93\n\
\3-94,4-94\n\
\51-57,46-57\n\
\3-31,2-85\n\
\51-55,51-54\n\
\10-76,75-76\n\
\20-64,49-62\n\
\8-99,26-99\n\
\3-55,2-2\n\
\56-77,56-85\n\
\31-82,1-81\n\
\71-76,70-77\n\
\40-52,53-77\n\
\8-20,9-9\n\
\19-41,40-40\n\
\18-64,19-46\n\
\85-85,23-86\n\
\15-39,8-19\n\
\57-64,56-70\n\
\48-81,49-80\n\
\17-92,1-93\n\
\29-57,35-56\n\
\43-98,42-97\n\
\47-90,17-91\n\
\73-73,72-72\n\
\20-72,44-71\n\
\2-95,2-96\n\
\77-79,48-77\n\
\17-88,18-92\n\
\5-80,5-6\n\
\1-49,50-64\n\
\34-94,35-93\n\
\2-22,3-23\n\
\12-34,11-34\n\
\46-55,40-55\n\
\55-61,51-60\n\
\2-70,3-69\n\
\8-94,2-13\n\
\43-60,47-60\n\
\19-20,19-36\n\
\64-72,63-65\n\
\11-45,10-45\n\
\23-87,22-87\n\
\10-89,9-90\n\
\1-1,2-97\n\
\42-42,41-42\n\
\12-95,11-95\n\
\5-95,95-98\n\
\2-99,3-61\n\
\80-96,65-96\n\
\41-94,41-42\n\
\50-70,50-71\n\
\36-51,50-52\n\
\36-85,9-85\n\
\16-24,16-23\n\
\19-96,20-85\n\
\89-90,15-90\n\
\7-99,5-93\n\
\17-26,16-59\n\
\92-92,60-93\n\
\35-80,35-90\n\
\1-3,3-91\n\
\78-89,78-88\n\
\4-29,5-28\n\
\20-60,15-79\n\
\55-95,56-62\n\
\58-97,59-98\n\
\7-15,16-88\n\
\15-43,16-48\n\
\3-96,4-95\n\
\34-35,2-34\n\
\1-73,5-73\n\
\1-97,4-97\n\
\63-96,62-98\n\
\76-78,77-78\n\
\46-46,27-47\n\
\11-11,12-50\n\
\15-86,90-90\n\
\54-74,53-74\n\
\5-93,5-5\n\
\14-85,15-85\n\
\36-92,91-91\n\
\50-74,51-62\n\
\51-51,17-52\n\
\10-79,1-80\n\
\74-92,18-91\n\
\17-24,23-51\n\
\33-61,9-62\n\
\6-97,2-98\n\
\72-96,95-97\n\
\43-80,33-80\n\
\17-34,16-35\n\
\60-83,59-83\n\
\49-65,48-65\n\
\70-71,18-71\n\
\10-87,66-86\n\
\19-69,20-69\n\
\24-59,25-58\n\
\11-36,12-35\n\
\47-99,48-99\n\
\14-96,96-97\n\
\22-96,22-85\n\
\68-68,17-69\n\
\5-71,4-70\n\
\23-48,22-49\n\
\88-90,3-88\n\
\29-55,37-56\n\
\41-93,40-92\n\
\91-91,37-92\n\
\2-96,3-21\n\
\40-76,75-75\n\
\2-8,8-96\n\
\8-96,99-99\n\
\52-83,82-82\n\
\15-92,80-86\n\
\10-87,6-87\n\
\15-29,14-30\n\
\13-86,8-85\n\
\9-95,41-44\n\
\2-44,3-44\n\
\8-91,70-87\n\
\28-66,28-67\n\
\2-86,1-3\n\
\3-93,2-94\n\
\11-18,8-19\n\
\12-69,13-70\n\
\4-13,8-12\n\
\86-98,2-86\n\
\75-91,62-79\n\
\68-84,67-83\n\
\3-94,2-94\n\
\80-98,80-98\n\
\54-89,54-55\n\
\69-97,35-88\n\
\39-99,38-89\n\
\9-91,10-92\n\
\16-17,16-86\n\
\7-11,7-8\n\
\7-57,7-8\n\
\2-41,2-39\n\
\18-63,17-64\n\
\81-85,83-85\n\
\97-97,69-98\n\
\1-4,4-61\n\
\90-93,58-93\n\
\34-73,33-73\n\
\7-87,86-88\n\
\19-95,18-96\n\
\4-5,5-77\n\
\35-35,35-54\n\
\43-43,42-82\n\
\5-99,3-6\n\
\14-29,15-65\n\
\82-86,2-83\n\
\8-20,9-15\n\
\2-97,3-97\n\
\69-88,70-88\n\
\38-38,37-92\n\
\17-42,17-41\n\
\80-80,79-94\n\
\24-45,24-72\n\
\51-62,50-89\n\
\5-72,6-61\n\
\56-78,16-51\n\
\71-84,70-71\n\
\2-78,13-68\n\
\63-92,12-73\n\
\40-90,37-79\n\
\47-93,49-93\n\
\21-72,3-73\n\
\96-97,3-97\n\
\7-74,50-75\n\
\58-87,86-87\n\
\88-90,4-89\n\
\16-16,17-29\n\
\4-97,10-97\n\
\22-92,22-91\n\
\88-89,11-89\n\
\37-87,37-94\n\
\75-75,15-76\n\
\20-77,76-76\n\
\32-80,33-75\n\
\17-68,18-69\n\
\93-98,4-93\n\
\19-20,19-79\n\
\41-60,42-61\n\
\92-93,2-93\n\
\6-99,7-99\n\
\52-94,53-94\n\
\61-91,40-92\n\
\76-76,15-77\n\
\17-71,16-78\n\
\23-35,35-48\n\
\1-23,22-99\n\
\11-50,4-80\n\
\5-95,95-96\n\
\98-98,1-99\n\
\13-72,71-71\n\
\35-94,34-34\n\
\1-98,1-97\n\
\9-69,17-69\n\
\22-55,21-55\n\
\42-85,26-41\n\
\14-81,21-82\n\
\24-53,40-53\n\
\76-80,59-77\n\
\16-22,17-23\n\
\5-38,5-6\n\
\18-20,18-19\n\
\3-62,61-63\n\
\8-63,4-63\n\
\21-39,22-40\n\
\43-91,90-92\n\
\35-51,50-51\n\
\14-82,34-82\n\
\34-58,57-57\n\
\20-68,21-68\n\
\16-16,15-72\n\
\38-40,37-41\n\
\46-51,9-48\n\
\4-96,4-97\n\
\30-83,30-41\n\
\3-9,9-10\n\
\72-73,39-72\n\
\6-95,94-94\n\
\58-71,57-72\n\
\11-86,10-55\n\
\9-79,10-78\n\
\12-92,5-93\n\
\10-85,68-85\n\
\11-95,10-12\n\
\11-37,36-56\n\
\28-87,87-97\n\
\21-96,20-20\n\
\71-80,18-81\n\
\23-24,2-23\n\
\7-9,9-38\n\
\37-55,3-55\n\
\72-86,54-87\n\
\48-94,11-94\n\
\68-92,69-87\n\
\51-53,50-53\n\
\39-88,40-87\n\
\3-12,1-13\n\
\5-82,2-82\n\
\5-67,1-68\n\
\94-94,16-95\n\
\31-31,32-96\n\
\9-99,10-99\n\
\62-64,2-63\n\
\11-98,11-97\n\
\15-36,27-35\n\
\39-41,39-40\n\
\11-16,11-15\n\
\14-16,15-53\n\
\14-90,14-91\n\
\20-81,20-82\n\
\59-91,39-90\n\
\39-83,38-83\n\
\76-78,43-77\n\
\69-91,15-69\n\
\67-85,37-84\n\
\20-63,52-63\n\
\38-68,37-38\n\
\33-34,33-92\n\
\37-66,66-83\n\
\3-99,2-99\n\
\77-79,74-79\n\
\45-76,46-75\n\
\59-95,40-53\n\
\10-65,64-66\n\
\41-92,42-56\n\
\6-77,6-77\n\
\2-91,2-3\n\
\16-75,16-83\n\
\64-93,63-92\n\
\13-91,8-90\n\
\12-63,13-13\n\
\98-99,4-99\n\
\60-81,29-61\n\
\31-96,96-96\n\
\89-90,5-90\n\
\34-39,37-40\n\
\73-88,74-89\n\
\37-59,46-59\n\
\23-85,24-84\n\
\8-31,6-32\n\
\16-55,15-16\n\
\37-37,10-77\n\
\39-85,84-86\n\
\2-80,2-82\n\
\11-66,5-12\n\
\34-48,33-35\n\
\8-88,1-73\n\
\39-91,40-92\n\
\69-93,68-95\n\
\18-49,18-77\n\
\89-91,90-91\n\
\98-98,18-98\n\
\14-93,15-92\n\
\11-12,11-46\n\
\25-81,26-80\n\
\27-95,26-27\n\
\14-75,74-76\n\
\53-80,53-81\n\
\43-95,43-96\n\
\27-89,84-90\n\
\13-96,14-89\n\
\12-13,13-47\n\
\45-81,44-49\n\
\95-97,5-96\n\
\31-31,32-94\n\
\74-84,39-85\n\
\7-93,8-92\n\
\30-61,62-97\n\
\22-72,22-69\n\
\58-74,59-75\n\
\1-3,2-61\n\
\4-17,11-16\n\
\52-52,51-53\n\
\31-93,4-92\n\
\23-85,84-85\n\
\17-98,16-97\n\
\8-32,9-48\n\
\7-90,89-90\n\
\12-98,12-87\n\
\62-88,61-61\n\
\34-89,31-88\n\
\13-15,14-98\n\
\9-37,38-78\n\
\23-43,22-44\n\
\7-30,30-31\n\
\43-43,42-84\n\
\2-98,1-98\n\
\15-84,14-85\n\
\45-84,84-84\n\
\14-14,15-42\n\
\2-92,2-91\n\
\90-91,23-90\n\
\35-56,34-34\n\
\2-91,4-90\n\
\34-97,8-97\n\
\9-75,9-75\n\
\3-17,18-98\n\
\55-91,55-92\n\
\54-95,55-99\n\
\42-43,43-74\n\
\34-42,33-42\n\
\13-79,79-79\n\
\15-85,16-85\n\
\25-30,29-29\n\
\19-21,20-21\n\
\37-72,36-48\n\
\27-52,1-53\n\
\27-34,33-33\n\
\1-72,4-72\n\
\28-67,66-66\n\
\61-64,64-91\n\
\42-75,68-74\n\
\96-97,23-96\n\
\3-29,4-13\n\
\13-90,56-90\n\
\7-89,8-89\n\
\21-23,22-47\n\
\95-96,57-96\n\
\59-59,20-59\n\
\4-5,4-90\n\
\31-86,30-85\n\
\13-91,14-91\n\
\62-70,34-36\n\
\66-67,12-67\n\
\4-5,5-33\n\
\10-54,9-55\n\
\73-82,50-73\n\
\59-68,60-68\n\
\36-63,35-47\n\
\30-87,31-86\n\
\18-50,6-51\n\
\18-64,17-64\n\
\87-88,14-88\n\
\66-66,35-75\n\
\27-59,5-28\n\
\6-53,1-52\n\
\12-84,13-83\n\
\36-74,53-75\n\
\9-90,13-90\n\
\94-96,62-95\n\
\44-58,44-86\n\
\26-81,82-92\n\
\1-38,18-39\n\
\81-81,39-80\n\
\9-44,8-45\n\
\15-84,12-15\n\
\6-14,5-76\n\
\15-88,15-87\n\
\59-95,59-92\n\
\78-98,28-78\n\
\66-77,65-76\n\
\49-63,19-64\n\
\1-83,22-64\n\
\19-91,20-92\n\
\14-80,13-80\n\
\14-56,43-55\n\
\29-68,36-69\n\
\46-72,47-73\n\
\14-28,6-27\n\
\39-54,39-40\n\
\37-72,37-38\n\
\96-98,11-97\n\
\77-96,96-97\n\
\47-64,60-91\n\
\1-94,3-93\n\
\81-98,56-98\n\
\75-88,88-88\n\
\40-94,93-93\n\
\19-97,19-99\n\
\18-18,19-97\n\
\49-96,48-95\n\
\5-93,92-93\n\
\17-18,17-84\n\
\77-77,23-77\n\
\25-27,6-27\n\
\6-98,98-98\n\
\17-67,17-66\n\
\4-34,4-50\n\
\40-41,40-99\n\
\38-96,38-95\n\
\23-44,23-44\n\
\38-59,58-58\n\
\88-89,88-88\n\
\40-46,39-83\n\
\66-76,23-76\n\
\15-64,16-65\n\
\6-36,36-37\n\
\17-53,17-52\n\
\41-79,42-78\n\
\1-71,70-71\n\
\5-87,45-87\n\
\25-29,7-29\n\
\9-16,10-26\n\
\69-85,67-86\n\
\25-57,8-57\n\
\2-56,3-55\n\
\57-76,58-93\n\
\7-7,6-94\n\
\26-93,27-94\n\
\3-89,34-88\n\
\52-70,51-65\n\
\7-73,59-72\n\
\66-99,47-65\n\
\16-45,26-45\n\
\72-79,71-78\n\
\79-79,20-80\n\
\23-90,22-91\n\
\71-96,70-96\n\
\68-88,11-72\n\
\31-76,86-87\n\
\13-93,12-94\n\
\20-98,20-98\n\
\6-91,9-90\n\
\7-92,20-86\n\
\11-90,11-62\n\
\8-89,9-88\n\
\31-98,32-94\n\
\19-19,18-73\n\
\31-70,1-71\n\
\2-81,33-82\n\
\11-65,76-83\n\
\11-89,90-97\n\
\81-82,39-82\n\
\1-96,3-95\n\
\9-98,8-98\n\
\22-25,23-25\n\
\16-73,14-72\n\
\5-79,44-80\n\
\95-95,10-96\n\
\1-90,1-97\n\
\13-73,13-72\n\
\95-96,23-96\n\
\4-46,37-47\n\
\19-34,19-99\n\
\50-91,19-90\n\
\5-72,13-68\n\
\16-71,15-72\n\
\8-95,89-94\n\
\4-97,1-98\n\
\45-89,44-90\n\
\35-36,35-97\n\
\28-79,16-78\n\
\88-93,88-92\n\
\18-85,89-92\n\
\20-91,19-92\n\
\6-49,12-48\n\
\12-46,5-51\n\
\36-98,75-97\n\
\6-21,4-21\n\
\5-83,60-84\n\
\40-95,39-96\n\
\96-97,17-97\n\
\21-94,21-72\n\
\19-90,10-20\n\
\9-21,22-65\n\
\4-55,4-54\n\
\8-86,85-85\n\
\3-82,3-81\n\
\36-54,37-53\n\
\15-87,14-14\n\
\7-53,8-52\n\
\96-96,13-97\n\
\3-72,71-71\n\
\1-79,5-78\n\
\23-55,16-60\n\
\15-41,41-76\n\
\64-69,64-97\n\
\3-99,3-99\n\
\3-93,2-93\n\
\16-94,17-94\n\
\10-94,9-94\n\
\26-96,15-96\n\
\16-52,19-52\n\
\12-91,12-92\n\
\52-95,51-99\n\
\2-48,2-3\n\
\19-59,13-58\n\
\6-90,7-7\n\
\73-95,74-99\n\
\7-74,74-74\n\
\21-44,33-45\n\
\41-89,40-42\n\
\94-96,80-95\n\
\77-86,82-85\n\
\53-93,58-92\n\
\5-98,97-97\n\
\9-71,9-72\n\
\6-84,6-84\n\
\90-94,73-84\n\
\1-98,2-9\n\
\20-88,52-87\n\
\9-89,9-96\n\
\12-84,11-11\n\
\2-29,1-2\n\
\2-98,1-99\n\
\44-45,31-44\n\
\7-77,8-98\n\
\3-4,3-6\n\
\2-20,4-21\n\
\36-63,62-62\n\
\1-98,8-99\n\
\43-82,81-81\n\
\18-30,17-29\n\
\49-94,14-49\n\
\28-60,1-59\n\
\2-98,3-98\n\
\22-38,37-39\n\
\2-79,78-95\n\
\25-30,24-31\n\
\1-82,4-82\n\
\4-84,4-83\n\
\8-96,8-8\n\
\27-67,27-68\n\
\9-27,17-27\n\
\8-70,69-69\n\
\19-20,20-88\n\
\13-79,47-79\n\
\2-67,3-68\n\
\8-60,36-60\n\
\29-33,32-34\n\
\9-94,8-9\n\
\15-28,15-28\n\
\1-2,2-53\n\
\43-47,8-55\n\
\26-82,25-26\n\
\10-97,96-96\n\
\28-33,27-39\n\
\77-81,76-82\n\
\28-78,67-79\n\
\14-80,48-79\n\
\76-77,59-76\n\
\11-11,10-89\n\
\8-30,5-17\n\
\11-75,12-75\n\
\13-90,55-91\n\
\14-15,14-39\n\
\94-96,23-95\n\
\20-84,20-89\n\
\28-95,29-94\n\
\3-55,1-54\n\
\3-98,3-97\n\
\44-89,43-89\n\
\58-96,57-74\n\
\26-85,26-26\n\
\52-53,52-91\n\
\2-95,6-96\n\
\3-87,87-97\n\
\9-25,7-26\n\
\8-76,14-75\n\
\14-38,7-43\n\
\66-67,66-80\n\
\49-98,48-50\n\
\77-96,77-97\n\
\29-89,27-28\n\
\42-92,42-91\n\
\9-71,8-8\n\
\40-40,1-39\n\
\26-27,26-89\n\
\77-78,17-77\n\
\20-33,31-34\n\
\64-65,23-65\n\
\31-62,32-60\n\
\61-81,61-80\n\
\57-87,87-87"

lns = lines input
val (Just a) = a

pair ch text =
    let i = findIndex (==ch) text
        (l, r) = splitAt (val i) text
    in  (l, tail r)

parse line =
    let (first, second) = pair ',' line
        g a =
            let (f, t) = pair '-' a
            in  (read f, read t) :: (Int, Int)
    in  (g first, g second)

contains (a,b) (c,d) = a <= c && d <= b

test (a,b) = a `contains` b || b `contains` a

answer1 = length $ filter test $ map parse lns

---

overlaps ((a,b), (c,d)) = 
    c <= a && a <= d ||
    c <= b && b <= d ||
    a <= c && c <= b ||
    a <= d && d <= b

answer2 = length $ filter overlaps $ map parse lns