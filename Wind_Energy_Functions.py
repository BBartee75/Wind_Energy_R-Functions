#---------------------------------------------------------------------------------------------------------------------------------------
# 
#---------------------------------------------------------------------------------------------------------------------------------------

import pandas as pd
import numpy as np
import os
import zipfile
import matplotlib.pyplot as plt
from jinja2 import Template
from datetime import datetime

def Power_Curve_Charts_Render(file_path, site_name, wtg_data_yr, ul_image_path, wtg_type, max_cap):
    """
    Renders power curve charts and outputs for each WTG, saving the file as an HTML file
    and creating a zip file to hold the HTML file and the image.

    This function requires pandas, numpy, matplotlib, and jinja2.

    Args:
        file_path (str): The path to the directory where the output folder will be created.
        site_name (str): The name of the site.
        wtg_data_yr (pd.DataFrame): DataFrame containing wind turbine data.
        ul_image_path (str): Path to the UL image file (e.g., 'ULimage.PNG').
        wtg_type (str): The type of wind turbine.
        max_cap (float): The maximum capacity of the wind turbine in kW.
    """
    # Verify data availability
    if not isinstance(wtg_data_yr, pd.DataFrame) or wtg_data_yr.empty:
        raise ValueError("Error: Required DataFrame 'wtg_data_yr' is missing or empty.")
    if not os.path.exists(ul_image_path):
        raise FileNotFoundError("Error: Required image file 'ULimage.PNG' not found at the specified path.")
    if not wtg_type:
        raise ValueError("Error: 'wtg_type' must be specified.")
    if not isinstance(max_cap, (int, float)):
        raise ValueError("Error: 'max_cap' must be a numeric value.")

    # Create folder to export data
    folder_path = os.path.join(file_path, "OutPut", site_name)
    os.makedirs(folder_path, exist_ok=True)

    # Copy the UL image to the output folder
    ul_image_basename = os.path.basename(ul_image_path)
    dest_ul_image_path = os.path.join(folder_path, ul_image_basename)
    with open(ul_image_path, 'rb') as f_src, open(dest_ul_image_path, 'wb') as f_dst:
        f_dst.write(f_src.read())

    # Generate plots and store paths
    wtg_list = sorted(wtg_data_yr['WTG'].unique())
    plot_image_paths = []
    for wtg in wtg_list:
        wtg_subset = wtg_data_yr[(wtg_data_yr['WTG'] == wtg) & (wtg_data_yr['RealPower'] > 0)]
        if wtg_subset.empty:
            print(f"No data found for WTG-{wtg}. Skipping this WTG.")
            continue

        fig, ax = plt.subplots(figsize=(8, 6))
        ax.scatter(wtg_subset['WindSpeed'], wtg_subset['RealPower'], color='#00689C')
        
        # Smooth line
        x_smooth = np.linspace(wtg_subset['WindSpeed'].min(), wtg_subset['WindSpeed'].max(), 500)
        y_smooth = np.interp(x_smooth, wtg_subset['WindSpeed'], wtg_subset['RealPower'])
        ax.plot(x_smooth, y_smooth, color='#04B404', linewidth=1.1)

        # Rated power line
        rating = wtg_subset['Rating'].iloc[0]
        ax.axhline(y=rating, color='#CA0123', linestyle='-.', linewidth=0.65, label=f"Rated Power\n {rating} (kW)")
        ax.legend(loc='lower right', title="", fontsize=8)

        ax.set_title(f"WTG-{wtg} Power Curve Year 2023", fontsize=10)
        ax.set_xlabel("Windspeed", fontsize=8)
        ax.set_ylabel("RealPower", fontsize=8)

        ax.set_ylim(0, max_cap + 100)
        ax.set_xlim(wtg_subset['WindSpeed'].min(), wtg_subset['WindSpeed'].max())
        ax.tick_params(axis='both', which='major', labelsize=6)
        
        # Add annotation for wtgType
        ax.text(ax.get_xlim()[1] * 0.95, rating, wtg_type, horizontalalignment='right',
                verticalalignment='bottom', color='#04B404', fontsize=8)

        plot_filename = f"power_curve_wtg_{wtg}.png"
        plot_path = os.path.join(folder_path, plot_filename)
        plt.savefig(plot_path, bbox_inches='tight')
        plt.close(fig)
        plot_image_paths.append(plot_path)

    # HTML template content
    html_template = """
    <!DOCTYPE html>
    <html>
    <head>
        <title>{{ title }}</title>
        <style>
            body { font-family: sans-serif; }
            .header-image { float: right; }
            .plot-container { page-break-inside: avoid; margin-bottom: 20px; }
            .plot-image { width: 100%; height: auto; }
        </style>
    </head>
    <body>
        <div class="header-image">
            <img src="{{ ul_image_name }}" width="200px" align="right">
        </div>
        <h1>{{ title }}</h1>
        <p>Author: UL Solutions Asset Advisory Services</p>
        <p>Date: {{ date }}</p>
        <hr>
        <h2>Yearly Charts</h2>
        <p>Note these are all 10 min timestamps. Find each WTG and loop for each WTG to create a power curve chart. We filter for all realpower that is > 0.</p>
        {% for plot in plots %}
        <div class="plot-container">
            <img src="{{ plot }}" class="plot-image" alt="Power curve plot">
        </div>
        {% endfor %}
    </body>
    </html>
    """

    # Render HTML
    template = Template(html_template)
    html_content = template.render(
        title=f"{site_name} Yearly Graphs",
        date=datetime.now().strftime('%B %d, %Y'),
        ul_image_name=ul_image_basename,
        plots=[os.path.basename(p) for p in plot_image_paths]
    )

    html_filename = f"{datetime.now().strftime('%Y-%m-%d')}_{site_name}_YearlyGraphs.html"
    html_file_path = os.path.join(folder_path, html_filename)
    with open(html_file_path, 'w') as f:
        f.write(html_content)

    # Create a zip file
    zip_filename = f"{datetime.now().strftime('%Y-%m-%d')}_{site_name}_YearlyGraphs.zip"
    zip_file_path = os.path.join(folder_path, zip_filename)
    with zipfile.ZipFile(zip_file_path, 'w', zipfile.ZIP_DEFLATED) as zipf:
        zipf.write(html_file_path, os.path.basename(html_file_path))
        zipf.write(dest_ul_image_path, os.path.basename(dest_ul_image_path))
        for plot_path in plot_image_paths:
            zipf.write(plot_path, os.path.basename(plot_path))

    # Clean up temporary files
    os.remove(html_file_path)
    for plot_path in plot_image_paths:
        os.remove(plot_path)

    print(f"\n*** YearlyGraphs report is rendered and complete. Zip file created at {zip_file_path} ***\n")
	
	
	
#---------------------------------------------------------------------------------------------------------------------------------------
# This function calculates the wind turbulence, which is the standard deviation of wind speed divided by the mean wind speed.
#---------------------------------------------------------------------------------------------------------------------------------------

import numpy as np
import pandas as pd

def calculate_Wind_Turbulence(data, wind_speed_column):
    """
    Calculates wind turbulence (standard deviation of wind speed / mean wind speed)
    and adds it as a 'Turbulence' column to the input DataFrame.

    Args:
        data (pd.DataFrame): The input DataFrame.
        wind_speed_column (str): The name of the wind speed column.

    Returns:
        pd.DataFrame: The DataFrame with the new 'Turbulence' column.
    """
    if wind_speed_column not in data.columns:
        raise ValueError(f"Error: Wind speed column '{wind_speed_column}' not found in the DataFrame.")

    wind_speed_sd = np.std(data[wind_speed_column].dropna())
    mean_wind_speed = np.mean(data[wind_speed_column].dropna())

    if mean_wind_speed == 0:
        data['Turbulence'] = np.nan
        print("Warning: Mean wind speed is zero, turbulence set to NA.")
    else:
        data['Turbulence'] = wind_speed_sd / mean_wind_speed

    return data	

#---------------------------------------------------------------------------------------------------------------------------------------
# This function calculates air density for each row of the input DataFrame using the ideal gas law.
#---------------------------------------------------------------------------------------------------------------------------------------	

import pandas as pd

def calculate_air_density(data, temp_col, pressure_col):
    """
    Calculates air density for each row of the input DataFrame and adds it as an 'AirDensity' column.
    This implementation uses the ideal gas law.

    Args:
        data (pd.DataFrame): The input DataFrame.
        temp_col (str): The name of the air temperature column (in Celsius).
        pressure_col (str): The name of the air pressure column (in hectopascals, hPa or mb).

    Returns:
        pd.DataFrame: The DataFrame with the new 'AirDensity' column.
    """
    if temp_col not in data.columns:
        raise ValueError(f"Error: Air temperature column '{temp_col}' not found in the DataFrame.")
    if pressure_col not in data.columns:
        raise ValueError(f"Error: Air pressure column '{pressure_col}' not found in the DataFrame.")

    # Convert pressure to Pascals (1 hPa = 100 Pa)
    # Convert temperature to Kelvin (C + 273.15)
    # R (gas constant for dry air) is 287.05 J/(kg·K)
    data['AirDensity'] = (data[pressure_col] * 100) / (287.05 * (data[temp_col] + 273.15))
    return data


#---------------------------------------------------------------------------------------------------------------------------------------
# This function normalizes wind speed to a standard air density.
#---------------------------------------------------------------------------------------------------------------------------------------	

import pandas as pd

def normalize_wind_speed(data, wind_speed_col="wind.speed", air_density_col="AirDensity", standard_air_density=1.225):
    """
    Normalizes wind speed to a standard air density.

    Args:
        data (pd.DataFrame): The input DataFrame.
        wind_speed_col (str): The name of the wind speed column.
        air_density_col (str): The name of the air density column.
        standard_air_density (float): The standard air density in kg/m^3.

    Returns:
        pd.Series: The normalized wind speed data.
    """
    if wind_speed_col not in data.columns:
        raise ValueError(f"Error: Wind speed column '{wind_speed_col}' not found in the DataFrame.")
    if air_density_col not in data.columns:
        raise ValueError(f"Error: Air density column '{air_density_col}' not found in the DataFrame.")

    normalized_ws = data[wind_speed_col] * ((data[air_density_col] / standard_air_density) ** (1 / 3))
    return normalized_ws


#---------------------------------------------------------------------------------------------------------------------------------------
# This function calculates air density and density-adjusts wind speed based on various inputs like temperature, pressure, and elevation.
#---------------------------------------------------------------------------------------------------------------------------------------	

import numpy as np

def density_Adj_WindSpeed(ws, temperature, pressure=1013.25, rh=0.5, elevation=0, norm_rho=0):
    """
    Calculates air density and density adjusts wind speed data based on the input temperature.

    Args:
        ws (np.ndarray or list): Wind speed in m/s.
        temperature (np.ndarray or list): Temperature in Celsius.
        pressure (float or np.ndarray or list): Pressure in millibars (mb).
        rh (float or np.ndarray or list): Relative humidity as a percentage of 1 (e.g., 0.5 for 50%).
        elevation (float or np.ndarray or list): Elevation in meters.
        norm_rho (float): The normalization density. If 0, the mean density is used.

    Returns:
        dict: A dictionary containing 'dc_ws' (density-adjusted wind speed) and 'density'.
    """
    ws = np.array(ws)
    temperature = np.array(temperature)
    pressure = np.array(pressure)
    rh = np.array(rh)
    
    # Air density calculation
    if np.all(pressure == 1013.25) and np.all(rh == 0.5):
        # Temp and elevation only
        density = (1013.25 * 100) / (287 * (temperature + 273.15)) * np.exp((-9.81 * elevation * (1.0397 - 0.000025 * elevation)) / (287 * (temperature + 273.15)))
    elif np.any(pressure != 1013.25) and np.any(rh != 0.5):
        # Pressure and RH included
        density = 1 / (temperature + 273.15) * (
            (pressure * 100 / 287.05) -
            (rh * (0.0000205 * np.exp(0.0631846 * (temperature + 273.15))) * (1 / 287.05 - 1 / 461.5))
        )
    else:
        # Just temperature and pressure
        density = pressure * 100 / 287 / (temperature + 273.15)

    # Apply density adjustment
    if norm_rho == 0:
        norm_rho = np.mean(density[~np.isnan(density)])

    adj_coe = (density / norm_rho) ** (1 / 3)
    dc_ws = ws * adj_coe

    return {"dc_ws": dc_ws, "density": density}
	
#---------------------------------------------------------------------------------------------------------------------------------------
# This function calculates the capacity factor based on measured power and reference power.
#---------------------------------------------------------------------------------------------------------------------------------------	

import pandas as pd

def calculate_capacity_factor(data, reference_power, measured_power_col):
    """
    Calculates the capacity factor based on measured power and reference power.

    Args:
        data (pd.DataFrame): The input DataFrame.
        reference_power (float): The rated power capacity of the wind turbine in kW.
        measured_power_col (str): The name of the measured power column (in kW).

    Returns:
        pd.Series: The capacity factor as a percentage.
    """
    if measured_power_col not in data.columns:
        raise ValueError(f"Error: Measured power column '{measured_power_col}' not found in the DataFrame.")

    # Calculate capacity factor assuming 1 hour timestamps (or consistent timestamps)
    # The original R function's TPE calculation is based on a fixed 24-hour period,
    # but the AEO is a single measurement. Assuming the intent is for AEO to be
    # the power measurement, the formula is simplified. A more accurate calculation
    # would involve summing AEO over a period and dividing by TPE over the same period.
    # The Python function mirrors the R logic.
    aeo = data[measured_power_col]
    cf = (aeo / (reference_power * 1000)) * 100 # Convert reference_power to kW
    return cf
	
#---------------------------------------------------------------------------------------------------------------------------------------
# This function reads a specific format of Excel file, cleans the data, and returns a pandas DataFrame.
#---------------------------------------------------------------------------------------------------------------------------------------	

import pandas as pd

def read_NextEraScada_xlsx(file_loc, xlsx_name, rating=0):
    """
    Reads a NextEra SCADA Excel file, cleans the data, and returns a DataFrame.

    Args:
        file_loc (str): The location of the file.
        xlsx_name (str): The name of the Excel file.
        rating (int or float): The rated power capacity of the wind turbine.

    Returns:
        pd.DataFrame: A cleaned DataFrame.
    """
    file_path = os.path.join(file_loc, xlsx_name)
    
    # Read the Excel file, skipping the first 10 rows
    ge_file = pd.read_excel(file_path, sheet_name=0, header=10, engine='openpyxl')
    
    # Correct column names
    ge_file = ge_file.iloc[:, :4]
    ge_file.columns = ["WTG", "TimeStamp10Min", "RealPower", "WindSpeed"]

    # Clean the WTG column values
    ge_file['WTG'] = ge_file['WTG'].str.replace('xml:space=\"preserve\">', '', regex=False)

    # Convert TimeStamp column to datetime
    ge_file['TimeStamp10Min'] = pd.to_datetime(ge_file['TimeStamp10Min'], origin='1900-01-01', unit='D')
    
    # Replace NA values with 0 in 'RealPower' and 'WindSpeed'
    ge_file[['RealPower', 'WindSpeed']] = ge_file[['RealPower', 'WindSpeed']].fillna(0)
    
    # Add the 'Rating' column
    ge_file['Rating'] = rating
    
    # Filter out wind speed values greater than 50 m/s
    ge_file = ge_file[ge_file['WindSpeed'] <= 50]

    # Remove rows where WTG is 0
    if (ge_file['WTG'] == 0).any():
        ge_file = ge_file[ge_file['WTG'] != 0]

    return ge_file
	
#---------------------------------------------------------------------------------------------------------------------------------------
# This function calculates the eastward (u) and northward (v) wind components and then determines the wind direction from those components.
#---------------------------------------------------------------------------------------------------------------------------------------	

import numpy as np

def calc_wind_components_and_direction(ws, wd):
    """
    This function calculates the eastward and northward wind components from wind speed and direction,
    and then determines the wind direction from these components.

    Args:
        ws (float or np.ndarray): Wind speed in m/s.
        wd (float or np.ndarray): Wind direction in degrees.

    Returns:
        dict: A dictionary with 'u' (zonal), 'v' (meridional) components, and 'wind_direction'.
    """
    # Convert degrees to radians
    wd_rad = wd * np.pi / 180
    
    # Calculate components. Wind is blowing FROM the direction.
    # Therefore, we use negative signs.
    u = -ws * np.sin(wd_rad)
    v = -ws * np.cos(wd_rad)
    
    # Calculate wind direction from components
    wind_direction = (270 - np.degrees(np.arctan2(v, u))) % 360
    
    return {"u": u, "v": v, "wind_direction": wind_direction}

#---------------------------------------------------------------------------------------------------------------------------------------
# This function calculates the smaller angular difference between two wind directions.
#---------------------------------------------------------------------------------------------------------------------------------------	

import numpy as np

def wind_Direction_delta(wind_dir_1, wind_dir_2):
    """
    Calculates the smaller angular difference between two wind directions,
    ensuring the result is always between 0 and 180 degrees.

    Args:
        wind_dir_1 (float or np.ndarray): First wind direction in degrees.
        wind_dir_2 (float or np.ndarray): Second wind direction in degrees.

    Returns:
        float or np.ndarray: The smaller angular difference in degrees.
    """
    d1_rad = np.radians(wind_dir_1)
    d2_rad = np.radians(wind_dir_2)

    delta_dir_rad = np.abs(d1_rad - d2_rad)
    
    # If the difference is greater than pi, take the other way around
    delta_dir_rad = np.where(delta_dir_rad > np.pi, 2 * np.pi - delta_dir_rad, delta_dir_rad)
    
    delta_dir_deg = np.degrees(delta_dir_rad)
    return delta_dir_deg

#---------------------------------------------------------------------------------------------------------------------------------------
# This function calculates the zonal (east/west) wind component.
#---------------------------------------------------------------------------------------------------------------------------------------	

import numpy as np

def calc_u_zonal_wind_speed(ws, wd):
    """
    A function to calculate the zonal wind speed (east/west component) from
    wind speed and direction vectors.

    Args:
        ws (float or np.ndarray): Wind speed in m/s.
        wd (float or np.ndarray): Wind direction in degrees.

    Returns:
        float or np.ndarray: The zonal wind speed component.
    """
    u = -ws * np.sin(np.radians(wd))
    return u

#---------------------------------------------------------------------------------------------------------------------------------------
# This function calculates the meridional (north/south) wind component
#---------------------------------------------------------------------------------------------------------------------------------------	

import numpy as np

def calc_v_meridional_wind_speed(ws, wd):
    """
    A function to calculate the meridional wind speed (north/south component) from
    wind speed and direction vectors.

    Args:
        ws (float or np.ndarray): Wind speed in m/s.
        wd (float or np.ndarray): Wind direction in degrees.

    Returns:
        float or np.ndarray: The meridional wind speed component.
    """
    v = -ws * np.cos(np.radians(wd))
    return v


#---------------------------------------------------------------------------------------------------------------------------------------
# This function calculates wind direction from the meridional and zonal wind components.
#---------------------------------------------------------------------------------------------------------------------------------------	

import numpy as np

def windDir_meridional_zonal_wind_direction(u, v):
    """
    A function to calculate wind direction from meridional and zonal wind vector components.

    Args:
        u (float or np.ndarray): Zonal wind component.
        v (float or np.ndarray): Meridional wind component.

    Returns:
        float or np.ndarray: The wind direction in degrees.
    """
    # Use arctan2 for accurate direction calculation, handling quadrants correctly.
    # The formula (270 - np.degrees(np.arctan2(v, u))) % 360 handles the conversion
    # from cartesian components (u, v) to meteorological wind direction.
    wd = (270 - np.degrees(np.arctan2(v, u))) % 360
    return wd