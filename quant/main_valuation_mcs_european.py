from valuation_mcs_european import valuation_mcs_european

if __name__ == "__main__":
    me_gbm = market_environment('me_gbm', dt.datetime(2015, 1, 1))
    me_gbm.add_constant('initial_value', 36.)
    me_gbm.add_constant('volatility', 0.2)
    me_gbm.add_constant('final_date', dt.datetime(2015, 12, 31))
    me_gbm.add_constant('currency', 'EUR')
    me_gbm.add_constant('frequency', 'M')
    me_gbm.add_constant('paths', 10000)
    csr = constant_short_rate('csr', 0.06)
    me_gbm.add_curve('discount_curve', csr)
    gbm = geometric_brownian_motion('gbm', me_gbm)
    me_call = market_environment('me_call', me_gbm.pricing_date)
    me_call.add_constant('strike', 40.)
    me_call.add_constant('maturity', dt.datetime(2015, 12, 31))
    me_call.add_constant('currency', 'EUR')

    payoff_func = 'np.maximum(maturity_value - strike, 0)'

    eur_call = valuation_mcs_european('eur_call', underlying=gbm,
    mar_env=me_call, payoff_func=payoff_func)
    %time eur_call.present_value()
    %time eur_call.delta()
    %time eur_call.vega()

    #present value of greeks
    %%time
    s_list = np.arange(34., 46.1, 2.)
    p_list = []; d_list = []; v_list = []
    for s in s_list:
    eur_call.update(initial_value=s)
    p_list.append(eur_call.present_value(fixed_seed=True))
    d_list.append(eur_call.delta())
    v_list.append(eur_call.vega())