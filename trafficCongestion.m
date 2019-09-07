
% input data
fRouteTravelTimes = '/Users/wall0159/code/govHack2019/Priority-Route-Bluetooth-Travel-Times-Mar-2019.csv';
% travel time in seconds
fColumnExplanations = '/Users/wall0159/code/govHack2019/Priority-Route-Bluetooth-field-descriptions.csv';
fRouteDefs = '/Users/wall0159/code/govHack2019/Priority-Route-Link-Details-2019.csv'; 
routeTravelTimes = readtable(fRouteTravelTimes);
routeDefs = readtable(fRouteDefs);

for i = 2:15 % [10, 11, 12]
    tmp = routeTravelTimes{:,i};
    tmp(tmp==0) = NaN;
    med = median(tmp(~isnan(tmp)));
    maxv = max(tmp(~isnan(tmp)));
    if maxv > med * 3
        figure
        hist(tmp, 30);
        title(num2str(i));
    end
end
close all
% there are 11 routes where the maximum transit time is more than 3x the
% median value


%% plot coords on a map
geoshow(routeDefs{:,3}, routeDefs{:,4})
Lon = routeDefs{:,3};
Lat = routeDefs{:,4};
figure;hold on
plot_google_map('MapScale', 1) 
plot(Lon,Lat)
