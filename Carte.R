rm(list = ls())

library(leaflet)
library(htmlwidgets)
library(htmltools)
library(leafpop)
library(rnaturalearth)

# Simulate your project data
n <- 10
dates <- sample(2021:2025, n, replace = TRUE)

domaines <- c("Sismique", "Écologie", "Acoustique", "Courantométrie", "Bathy")
services <- c(
  "Étude d'impact", "Relevé terrain", "Analyse acoustique",
  "Cartographie habitat", "Suivi espèces"
)

coords <- data.frame(
  lat = runif(n, 43, 49),
  lng = runif(n, -5, 8),
  html_files = sample(c("Fiche_projet_1.html", "Fiche_projet_2.html"), n, replace = TRUE),
  Dates = as.character(dates),
  Domaines = sample(domaines, n, replace = TRUE),
  Services = sample(services, n, replace = TRUE),
  icones = rep("Dot", n),
  stringsAsFactors = FALSE
)

# Create map base
countries <- ne_countries(scale = "medium", returnclass = "sf")

map <- leaflet(options = leafletOptions(minZoom = 3, maxZoom = 10)) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(provider = "Esri.OceanBasemap") %>%
  addPolygons(
    data = countries,
    label = ~name,
    weight = 1, color = "#555", opacity = 0.5,
    fillOpacity = 0.1,
    highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.3, bringToFront = TRUE)
  ) %>%
  setView(lng = 0, lat = 0, zoom = 2) %>%
  setMaxBounds(
    lng1 = -180, lat1 = -85,
    lng2 = 180, lat2 = 85
  )

# Icons
Icones <- iconList(
  Dot = makeIcon(
    iconUrl = "icone_carte.png",
    iconRetinaUrl = "Images_et_Logo/icone_carte.png",
    iconWidth = 40, iconHeight = 40,
    iconAnchorX = 20, iconAnchorY = 40
  )
)

# Convert coords to JavaScript-safe JSON
marker_data <- jsonlite::toJSON(coords, dataframe = "rows", auto_unbox = TRUE)

# Inject CSS
popup_css <- tags$style(HTML("
  .leaflet-popup-content-wrapper {
    background: transparent !important;
    box-shadow: none !important;
  }
  .leaflet-popup-tip {
    background: transparent !important;
  }
  .leaflet-control-layers label {
    white-space: nowrap !important;
    display: flex !important;
    align-items: center;
    gap: 6px;
    max-width: 100%;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  .filter-controls {
    position: absolute;
    top: 10px;
    left: 50px;
    z-index: 1000;
    background: white;
    padding: 8px;
    border-radius: 5px;
    box-shadow: 0 0 6px rgba(0,0,0,0.2);
    font-size: 14px;
  }
  .filter-controls label {
    margin-right: 10px;
  }
"))

# Add dynamic marker rendering and filtering
final_map <- map %>%
  htmlwidgets::prependContent(popup_css) %>%
  htmlwidgets::onRender(sprintf("
    function(el, x) {
      var map = this;
      var markerData = %s;
      var markers = [];
      var markerLayer = L.layerGroup().addTo(map);

      // Define custom icon
      var customIcon = L.icon({
        iconUrl: 'icone_carte.png',
        iconSize: [40, 40],
        iconAnchor: [20, 40]
      });

      markerData.forEach(function(d) {
        var popup = `<iframe src='file:///C:/Users/lheinr02/Desktop/Cohabys/Projets/SiteWeb/Output/${d.html_files}' width='540' height='550' style='border:none;'></iframe>`;
        var marker = L.marker([d.lat, d.lng], {icon: customIcon}).bindPopup(popup);
        marker._meta = {
          date: d.Dates,
          domaine: d.Domaines,
          service: d.Services
        };
        markerLayer.addLayer(marker);
        markers.push(marker);
      });

      // Create filtering UI
      var controlDiv = L.DomUtil.create('div', 'filter-controls');
      controlDiv.innerHTML = `
        <label>Date:
          <select id='filter-date'><option value='all'>Toutes</option></select>
        </label>
        <label>Domaine:
          <select id='filter-domaine'><option value='all'>Tous</option></select>
        </label>
        <label>Service:
          <select id='filter-service'><option value='all'>Tous</option></select>
        </label>
      `;
      L.DomEvent.disableClickPropagation(controlDiv);
      map.getContainer().appendChild(controlDiv);

      function populateFilter(id, values) {
        var select = document.getElementById(id);
        [...new Set(values)].sort().forEach(function(val) {
          var opt = document.createElement('option');
          opt.value = val;
          opt.text = val;
          select.appendChild(opt);
        });
      }

      populateFilter('filter-date', markerData.map(d => d.Dates));
      populateFilter('filter-domaine', markerData.map(d => d.Domaines));
      populateFilter('filter-service', markerData.map(d => d.Services));

      function updateMarkers() {
        var selectedDate = document.getElementById('filter-date').value;
        var selectedDomaine = document.getElementById('filter-domaine').value;
        var selectedService = document.getElementById('filter-service').value;

        markerLayer.clearLayers();

        markers.forEach(function(m) {
          var match =
            (selectedDate === 'all' || m._meta.date === selectedDate) &&
            (selectedDomaine === 'all' || m._meta.domaine === selectedDomaine) &&
            (selectedService === 'all' || m._meta.service === selectedService);
          if (match) {
            markerLayer.addLayer(m);
          }
        });
      }

      ['filter-date', 'filter-domaine', 'filter-service'].forEach(function(id) {
        document.getElementById(id).addEventListener('change', updateMarkers);
      });

      updateMarkers(); // initial filter
    }
  ", marker_data))

# Print the map

saveWidget(final_map, file = "Output/Projects_map.html")
