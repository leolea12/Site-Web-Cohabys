rm(list = ls())

library(leaflet)
library(htmlwidgets)
library(htmltools)
library(leafpop)
library(rnaturalearth)

htmlDependency(
  name = "leaflet-providers",
  version = "1.9.0",
  src = c(href = "https://unpkg.com/leaflet-providers"),
  script = "leaflet-providers.js"
)

# Simulate your project data
n <- 10
dates <- sample(2021:2025, n, replace = TRUE)

domaines <- c("Sismique", "Écologie", "Acoustique", "Courantométrie", "Bathymétrie")
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
  addProviderTiles(provider = "Esri.WorldImagery") %>%
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
     display:none;
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
      let openPopup = null;

      // Define tile layers
      var defaultBasemap = L.tileLayer.provider('Esri.WorldImagery').addTo(map);
      var imageryBasemap = L.tileLayer.provider('Esri.OceanBasemap');
      var currentBasemap = defaultBasemap;

      // Function to switch basemap
      function switchBasemap(newBasemap) {
        if (currentBasemap !== newBasemap) {
          map.removeLayer(currentBasemap);
          map.addLayer(newBasemap);
          currentBasemap = newBasemap;
        }
      }

      // Define custom icon
      var customIcon = L.icon({
        iconUrl: 'icone_carte.png',
        iconSize: [40, 40],
        iconAnchor: [20, 40]
      });



      markerData.forEach(function(d) {
  var popupContent = `<iframe src='${d.html_files}' width='540' height='550' style='border:none;'></iframe>`;

  var marker = L.marker([d.lat, d.lng], {icon: customIcon});

  // create popup but don't bind to marker automatically
  var popup = L.popup({
  maxWidth: 560,
}).setContent(popupContent);

  marker._meta = {
    date: d.Dates,
    domaine: d.Domaines,
    service: d.Services
  };
  markerLayer.addLayer(marker);

marker.on('click', function(e) {
  map.flyTo(e.latlng, 10, {
    animate: true,
    duration: 3,
    easeLinearity: 0.9
  });

  // Close any open popup before starting a new one
  if (openPopup) {
    map.closePopup(openPopup);
    openPopup = null;
  }
  // Open popup only after the animation delay
    setTimeout(() => {
    var offsetLatLng = L.latLng(e.latlng.lat - 0.25, e.latlng.lng + 0.55);
    popup.setLatLng(offsetLatLng);
    popup.openOn(map);
    openPopup = popup;
  }, 3000);

  var domaine = this._meta.domaine;
  if (['Sismique', 'Acoustique', 'Courantométrie', 'Bathymétrie', 'Écologie'].includes(domaine)) {
    switchBasemap(imageryBasemap);
  } else {
    switchBasemap(defaultBasemap);
  }
});

  markers.push(marker);
});

      // Restore default basemap when zooming out
      map.on('zoomend', function() {
        if (map.getZoom() < 5) {
          switchBasemap(defaultBasemap)
        }
      })

      map.on('zoomend', function() {
        if (map.getZoom() < 10) {
          if (openPopup) {
            map.closePopup(openPopup)
            openPopup = null
          }
        }
      })

      // Restore basemap when clicking outside markers/popups
      map.on('click', function(e) {
        // If no popup is open, revert to default basemap
        if (!map._popup || !map._popup._isOpen) {
          switchBasemap(defaultBasemap);
        }
      });

      // Restore basemap when popup is closed
      map.on('popupclose', function(e) {
        switchBasemap(defaultBasemap);
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

final_map <- final_map %>%
  htmltools::attachDependencies(
    htmlDependency(
      name = "leaflet-providers",
      version = "1.9.0",
      src = c(href = "https://unpkg.com/leaflet-providers"),
      script = "leaflet-providers.js"
    )
  )

saveWidget(final_map, file = "docs/index.html", selfcontained = FALSE)
