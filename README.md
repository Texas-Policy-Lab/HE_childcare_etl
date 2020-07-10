# HE_childcare_etl

## Stage 2

### OSRM

OSRM (Open Source Routing Machine) is software developed to help people not only know the distance between two geographic coordinate points, but also the average duration of time between those two points.

To run the OSRM on docker, open a windows command prompt and run the following code. The code starts a server to run the osrm requests on. To get to this step, we took several other steps which are documented on [Confluence](https://tpldocs.rice.edu/display/HEC/OSRM).To extract the data required to run the osrm requests there was a lot of data to extract, which was done a more powerful computer (with more CPU and RAM). To replicate that process follow the steps outlined in Confluence, but be aware that memory and CPU settings may have to be adjusted.

If you running the following code it will open a server on port 5000. If you adjust which port the server is running on, make sure to adjust the port name in config.yaml as well.

Right now, the data used on the osrm process are stored in c:/docker. This is not the correct location to store these data, but docker is struggling with the mounted path (c:/26_HE/01_Childcare/Data), so the temporary fix is to put the data where we know docker looks for them.

#### Run docker command

1. Open a windows command prompt
2. Copy and paste the below docker command
3. You will know the server is running if you open the docker dashboards UI and a container named "osrm"

```
docker run --name osrm -t -i -p 5000:5000 -v c:/docker:/data osrm/osrm-backend osrm-routed --algorithm mld /data/texas-latest.osrm
```

#### Run the R script

Then run the osrm_main.R script. If you get an error message like "Failed to connect to port", then the container did not launch correctly.

#### Notes

The docker command we should be running:
```
docker run --name osrm -t -i -p 5000:5000 -v m:/26_HE/01_Childcare/Data:/data osrm/osrm-backend osrm-routed --algorithm mld /data/texas-latest.osrm
```

Nothing should have to be changed in the R script.

