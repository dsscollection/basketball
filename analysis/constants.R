##########
# App variables
##########
MAX_JUMP = 1

##########
# Player/team constants
##########
CLE_ID = 5
LBJ_P_ID = 214152
OKC_ID = 25
DURANT_P_ID = 329525
GSW_ID = 9
CURRY_P_ID = 338365
DET_ID = 8
DRUMMOND_P_ID = 614746

##########
# Raster variables
##########
XMIN = 0
XMAX = 94
XMID = (XMIN+XMAX)/2
YMIN = 0
YMAX = 50
NUM_CELLS = 600
RASTER_RES = 2

##########
# SportVU data variables
##########
# Entity Ids
BALL_ENTITY_ID = -1
# Event Ids
POSSESSION_ID = 23
FG_MISS_ID = 4
FG_MADE_ID = 3
OFF_REB_ID = 5
DEF_REB_ID = 6

##########
# Csv data format variables
##########
AWAY_EVTS_COLS = c('a1_event', 'a2_event', 'a3_event', 'a4_event', 'a5_event')
HOME_EVTS_COLS = c('h1_event', 'h2_event', 'h3_event', 'h4_event', 'h5_event')
AWAY_X_COLS = c('a1_x', 'a2_x', 'a3_x', 'a4_x', 'a5_x')
HOME_X_COLS = c('h1_x', 'h2_x', 'h3_x', 'h4_x', 'h5_x')
AWAY_Y_COLS = c('a1_y', 'a2_y', 'a3_y', 'a4_y', 'a5_y')
HOME_Y_COLS = c('h1_y', 'h2_y', 'h3_y', 'h4_y', 'h5_y')
AWAY_ENT_COLS = c('a1_ent', 'a2_ent', 'a3_ent', 'a4_ent', 'a5_ent')
HOME_ENT_COLS = c('h1_ent', 'h2_ent', 'h3_ent', 'h4_ent', 'h5_ent')
SHOT_BEGIN_EVTS = c(FG_MADE_ID, FG_MISS_ID)
SHOT_END_EVTS = c(POSSESSION_ID, DEF_REB_ID, OFF_REB_ID)

