from sn_random_numbers import sn_random_numbers
from geometric_brownian_motion import geometric_brownian_motion
from market_environment import*
import datetime as dt
from constant_short_rate import constant_short_rate

if __name__ == "__main__":
    me_gbm=market_environment('me_gbm', dt.datetime(2015, 1, 1))
    me_gbm.add_constant('initial_value', 36.)
    me_gbm.add_constant('volatility', 0.2)
    me_gbm.add_constant('final_date', dt.datetime(2015, 12, 31))
    me_gbm.add_constant('currency', 'EUR')
    me_gbm.add_constant('frequency', 'M')
    # monthly frequency (respective month end)
    me_gbm.add_constant('paths', 10000)
    csr=constant_short_rate('csr', 0.05)
    me_gbm.add_curve('discount_curve', csr)
    
    #instantiate a model simulation object
    gbm=geometric_brownian_motion('gbm', me_gbm)
    gbm
    
