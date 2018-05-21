def compute_degreeday()
    conn = uo.connect('weather_hourly_utc')
    with conn:
        df = pd.read_sql('SELECT * FROM downloaded', conn)

def main():
    return 0
    
main()
