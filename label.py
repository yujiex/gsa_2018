# lookup for label and title
ylabel_dict = {'eui':'Electricity + Gas [kBtu/sq.ft]',
               'eui_elec':'Electricity [kBtu/sq.ft]',
               'eui_gas':'Natural Gas [kBtu/sq.ft]',
               'eui_oil':'Oil [Gallons/sq.ft]',
               'eui_heat':'Gas+Oil+Steam [kBtu/sq.ft]',
               'all': 'Electricity-Gas [kBtu/sq.ft]',
               'eui_total': 'Total [kBtu/sq.ft]',
               'eui_water':'Water [Gallons/sq.ft]', 
               'electric': 'electric (kBtu/sq.ft)', 
               'gas': 'gas kBtu/sq.ft'
}

title_dict = {'eui':'Electricity + Gas',
              'eui_elec':'Electricity',
              'eui_gas':'Gas',
              'eui_oil':'Oil',
              'eui_heat':'Gas+Oil+Steam',
              'all': 'Combined Electricity and Gas',
              'eui_water':'Water'}

total_type_dict = {'elec_gas': 'Electricity + Gas',
                   'all_type': 'all energy type'}

kind_dict = {'temp': 'Temperature', 'hdd': 'HDD', 'cdd': 'CDD', 'all': 'Combined'}

# plot title
title_weather = {'eui':'Original and Weather Normalized '\
                      'Electricity + Gas Consumption',
                'eui_elec':'Original and Weather Normalized '
                           'Electricity Consumption',
                'eui_gas':'Original and Weather Normalized Natural '\
                          'Gas Consumption',
                'eui_oil':'Original and Weather Normalized Oil Consumption',
                'eui_water':'Original and Weather Normalized '\
                            'Water Consumption'}

title_dict_3 = {'eui':'Weather Normalized Electricity + Gas Consumption', 'eui_elec':'Weather Normalized Electricity Consumption', 'eui_gas':'Weather Normalized Natural Gas Consumption', 'eui_oil':'Weather Normalized Oil Consumption', 'eui_water':'Weather Normalized Water Consumption'}

xlabel_dict = {'temp': 'Monthly Mean Temperature, Deg F',
               'hdd': 'Monthly HDD, Deg F',
               'all': 'Monthly HDD(-)/CDD(+), Deg F',
               'cdd': 'Monthly CDD, Deg F'}
