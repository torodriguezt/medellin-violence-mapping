// map_nombres.js

// En tu shapefile el nombre del departamento está en esta columna:
const DEPT_NAME_FIELD = "DPTO_CNMBR";

// Variables globales
let map;
let insetMap;
let geojsonData = null;
let geojsonLayer = null;
let sanAndresLayer = null;

// Inicializar el mapa
function initMap() {
  map = L.map("map").setView([4.5, -74.1], 6);

  L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
    maxZoom: 19,
    attribution: '&copy; OpenStreetMap contributors'
  }).addTo(map);

  // Inicializar mapa inset (San Andrés)
  insetMap = L.map("map-inset", {
    zoomControl: false,
    attributionControl: false,
    dragging: false,
    scrollWheelZoom: false,
    doubleClickZoom: false,
    boxZoom: false,
    touchZoom: false,
    keyboard: false
  }).setView([12.55, -81.7], 10);

  L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
    maxZoom: 19
  }).addTo(insetMap);
}

// Normalizador de nombres
function normalizeName(str) {
  if (!str) return "";
  let s = str
    .toString()
    .toLowerCase()
    .normalize("NFD")
    .replace(/[\u0300-\u036f]/g, "")
    .replace(/[^\w\s]/g, " ")
    .replace(/\s+/g, " ")
    .trim();

  if (s.includes("archipielago de san andres")) {
    s = "san andres";
  }
  if (s.includes("san andres")) {
    s = "san andres";
  }
  if (s.includes("bogota")) {
    s = "bogota dc";
  }

  return s;
}

// Dibujar capa GeoJSON con nombres
function drawGeoJSON() {
  if (!map || !geojsonData) return;

  if (geojsonLayer) {
    map.removeLayer(geojsonLayer);
  }

  updateStatus("Datos cargados. Dibujando mapa...");

  // Estilo simple sin colores
  const getStyle = () => {
    return {
      fillColor: "#ffffff",
      fillOpacity: 0.3,
      color: "#374151",
      weight: 2
    };
  };

  // Capa principal
  geojsonLayer = L.geoJSON(geojsonData, {
    style: getStyle,
    onEachFeature: (feature, layer) => {
      const rawName = feature.properties[DEPT_NAME_FIELD];
      
      // Popup simple con el nombre
      layer.bindPopup(`<strong>${rawName || "Sin nombre"}</strong>`);

      // Detectar San Andrés
      const key = normalizeName(rawName);
      if (key.includes("san andres")) {
        sanAndresLayer = layer;
      }

      // Agregar etiqueta con el nombre del departamento
      if (rawName) {
        const center = layer.getBounds().getCenter();
        const marker = L.marker(center, {
          icon: L.divIcon({
            className: 'dept-label',
            html: rawName,
            iconSize: null
          })
        }).addTo(map);
      }
    }
  }).addTo(map);

  // Capa inset (solo visual con nombres)
  const insetLabels = [];
  L.geoJSON(geojsonData, {
    style: getStyle,
    onEachFeature: (feature, layer) => {
      const rawName = feature.properties[DEPT_NAME_FIELD];
      if (rawName) {
        const center = layer.getBounds().getCenter();
        const marker = L.marker(center, {
          icon: L.divIcon({
            className: 'dept-label',
            html: rawName,
            iconSize: null
          })
        });
        insetLabels.push(marker);
      }
    }
  }).addTo(insetMap);

  // Agregar las etiquetas al mapa inset
  insetLabels.forEach(label => label.addTo(insetMap));

  map.fitBounds(geojsonLayer.getBounds(), { padding: [20, 20] });

  // Centrar inset en San Andrés si se encontró
  if (sanAndresLayer) {
    const bounds = sanAndresLayer.getBounds();
    insetMap.fitBounds(bounds, { padding: [10, 10], maxZoom: 20 });
  }

  addSanAndresControl();
}

// Control de zoom específico para San Andrés
function addSanAndresControl() {
  if (!sanAndresLayer) return;

  const control = L.control({ position: "topleft" });

  control.onAdd = function () {
    const container = L.DomUtil.create("div", "leaflet-bar leaflet-control");
    
    // Botón San Andrés
    const btnSA = L.DomUtil.create("a", "", container);
    btnSA.innerHTML = "🏝️ San Andrés";
    btnSA.href = "#";
    btnSA.title = "Go to San Andrés";
    btnSA.style.display = "block";
    btnSA.style.padding = "5px 10px";
    btnSA.style.textDecoration = "none";
    btnSA.style.color = "black";
    btnSA.style.backgroundColor = "white";
    btnSA.style.fontSize = "12px";
    btnSA.style.fontWeight = "bold";

    L.DomEvent.on(btnSA, "click", (e) => {
      L.DomEvent.stop(e);
      const center = sanAndresLayer.getBounds().getCenter();
      map.setView(center, 11);
      sanAndresLayer.openPopup();
    });

    // Botón Reset
    const btnReset = L.DomUtil.create("a", "", container);
    btnReset.innerHTML = "🇨🇴 Colombia";
    btnReset.href = "#";
    btnReset.title = "Ver todo el país";
    btnReset.style.display = "block";
    btnReset.style.padding = "5px 10px";
    btnReset.style.textDecoration = "none";
    btnReset.style.color = "black";
    btnReset.style.backgroundColor = "white";
    btnReset.style.borderTop = "1px solid #ccc";
    btnReset.style.fontSize = "12px";
    btnReset.style.fontWeight = "bold";

    L.DomEvent.on(btnReset, "click", (e) => {
      L.DomEvent.stop(e);
      if (geojsonLayer) {
        map.fitBounds(geojsonLayer.getBounds(), { padding: [20, 20] });
      }
    });

    return container;
  };

  control.addTo(map);
}

// Status
function updateStatus(msg) {
  const el = document.getElementById("status");
  if (el) el.textContent = msg;
}

// Cargar datos automáticamente (solo Shapefile)
function loadData() {
  updateStatus("Cargando datos...");

  shp("../departamentos.zip")
    .then((geojson) => {
      geojsonData = geojson;
      updateStatus("Datos cargados. Dibujando mapa...");
      drawGeoJSON();
    })
    .catch(err => {
      console.error("Error loading data:", err);
      updateStatus("Error cargando datos: " + err.message);
    });
}

// Inicialización
document.addEventListener("DOMContentLoaded", () => {
  initMap();
  loadData();
});
