// Relative Risk Choropleth using Leaflet + D3
// Expects:
// - GeoJSON file of departments: prefer 'departamentos.geojson' or 'mgn_dptos.geojson' in repo root
// - CSV file: '../resultados/dat_inla_resultados.csv' (from repo root), with 'depto_norm' and a RR column

(function() {
  const geojsonCandidates = [
    '../departamentos.geojson',
    '../mgn_dptos.geojson'
  ];
  const csvPath = '../resultados/dat_inla_resultados.csv';

  const map = L.map('map', {
    zoomControl: true,
    attributionControl: true
  }).setView([4.5, -74.1], 5.5);

  L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
    maxZoom: 10,
    attribution: '&copy; OpenStreetMap contributors'
  }).addTo(map);

  const tooltip = d3.select('body').append('div').attr('class', 'tooltip').style('opacity', 0);

  function normalizeName(x) {
    if (!x) return '';
    let s = String(x).toLowerCase().trim();
    s = s.normalize('NFD').replace(/[\u0300-\u036f]/g, '');
    s = s.replace(/\s+/g, ' ');
    return s;
  }

  // Find a plausible name property in GeoJSON feature
  function getNameProp(props) {
    const candidates = ['DPTO_CNMBR','NOMBRE_DPT','NOMBRE_DEP','DEPARTAMEN','NOMBRE','NAM','name','Name'];
    for (const k of candidates) if (k in props) return k;
    // fallback to first string-like property
    for (const k of Object.keys(props)) {
      if (typeof props[k] === 'string') return k;
    }
    return null;
  }

  async function loadFirstAvailable(urls) {
    for (const u of urls) {
      try {
        const r = await fetch(u);
        if (r.ok) return r.json();
      } catch (e) { /* continue */ }
    }
    throw new Error('No GeoJSON candidate could be loaded: ' + urls.join(', '));
  }

  function detectRRColumn(columns) {
    const primary = ['relative_risk','RR','rr','rel_risk','riesgo_relativo','riesgo_rel'];
    const fallback = ['mean','Median','median','mu','fitted','expected'];
    for (const c of primary) if (columns.includes(c)) return c;
    for (const c of fallback) if (columns.includes(c)) return c;
    return null;
  }

  function makeColorScale(values) {
    // Robust domain: use percentiles to avoid outliers skewing the scale
    const v = values.filter(d => Number.isFinite(d));
    if (!v.length) return () => '#f0f0f0';
    v.sort((a,b) => a-b);
    const q10 = v[Math.floor(0.10 * (v.length-1))];
    const q90 = v[Math.floor(0.90 * (v.length-1))];
    const domainMin = Math.min(q10, d3.min(v));
    const domainMax = Math.max(q90, d3.max(v));
    return d3.scaleSequential(d3.interpolateMagma).domain([domainMin, domainMax]);
  }

  function addLegend(scale) {
    const legend = L.control({position: 'bottomright'});
    legend.onAdd = function () {
      const div = L.DomUtil.create('div', 'legend');
      const title = L.DomUtil.create('div', '', div);
      title.textContent = 'Relative Risk';
      const scaleRow = L.DomUtil.create('div', 'scale', div);
      // draw gradient
      const svg = d3.select(scaleRow).append('svg').attr('width', 140).attr('height', 14);
      const defs = svg.append('defs');
      const grad = defs.append('linearGradient').attr('id', 'legend-grad');
      grad.selectAll('stop')
        .data(d3.range(0, 1.01, 0.1))
        .enter().append('stop')
        .attr('offset', d => (d*100) + '%')
        .attr('stop-color', d => scale(scale.domain()[0] + d*(scale.domain()[1]-scale.domain()[0])));
      svg.append('rect').attr('x',0).attr('y',1).attr('width',140).attr('height',12).style('fill','url(#legend-grad)');
      const labels = L.DomUtil.create('div', '', div);
      labels.style.display = 'flex';
      labels.style.justifyContent = 'space-between';
      labels.style.fontSize = '12px';
      labels.style.marginTop = '2px';
      const minLabel = L.DomUtil.create('span', '', labels); minLabel.textContent = scale.domain()[0].toFixed(2);
      const maxLabel = L.DomUtil.create('span', '', labels); maxLabel.textContent = scale.domain()[1].toFixed(2);
      return div;
    };
    legend.addTo(map);
  }

  function attachTooltip(layer, rrMap, nameProp) {
    layer.on('mousemove', function(e) {
      const props = e.layer.feature.properties;
      const name = props[nameProp];
      const key = normalizeName(name);
      const rr = rrMap.get(key);
      tooltip.style('opacity', 1)
        .style('left', (e.originalEvent.pageX + 12) + 'px')
        .style('top', (e.originalEvent.pageY + 12) + 'px')
        .html(`<strong>${name || 'N/A'}</strong><br/>Relative Risk: ${rr != null ? rr.toFixed(3) : 'N/A'}`);
    });
    layer.on('mouseout', function() {
      tooltip.style('opacity', 0);
    });
  }

  Promise.all([
    loadFirstAvailable(geojsonCandidates),
    d3.csv(csvPath)
  ]).then(([geojson, csv]) => {
    if (!geojson || !geojson.features || !geojson.features.length) throw new Error('GeoJSON missing features');
    const sampleProps = geojson.features[0].properties || {};
    const nameProp = getNameProp(sampleProps);
    if (!nameProp) throw new Error('Could not detect a department name property in GeoJSON.');

    const columns = csv.columns || Object.keys(csv[0] || {});
    let rrCol = detectRRColumn(columns);
    if (!rrCol) throw new Error('Could not detect a relative risk column in CSV.');

    // Ensure depto_norm is present or create it from a name-like column
    const hasDeptoNorm = columns.includes('depto_norm');
    let nameColCSV = 'depto_norm';
    if (!hasDeptoNorm) {
      const candidates = ['depto','departamento','dept','DPTO_CNMBR','NOMBRE_DPT'];
      const found = candidates.find(c => columns.includes(c));
      if (!found) throw new Error('CSV has no depto_norm nor a recognizable department name column.');
      nameColCSV = found;
    }

    // Build map of normalized name -> RR
    const rrMap = new Map();
    csv.forEach(row => {
      const key = normalizeName(row[nameColCSV]);
      const val = +row[rrCol];
      if (key && Number.isFinite(val)) rrMap.set(key, val);
    });

    // Prepare color scale
    const vals = Array.from(rrMap.values());
    const color = makeColorScale(vals);

    // Count unmatched
    let unmatched = 0;
    const layer = L.geoJSON(geojson, {
      style: f => {
        const key = normalizeName(f.properties[nameProp]);
        const rr = rrMap.get(key);
        return {
          color: '#555',
          weight: 0.6,
          fillColor: rr != null ? color(rr) : '#f0f0f0',
          fillOpacity: 0.9
        };
      }
    }).addTo(map);

    layer.eachLayer(l => {
      const key = normalizeName(l.feature.properties[nameProp]);
      if (!rrMap.has(key)) unmatched++;
    });

    attachTooltip(layer, rrMap, nameProp);
    addLegend(color);

    // Fit bounds
    map.fitBounds(layer.getBounds(), { padding: [10,10] });

    if (unmatched) {
      console.warn(`Warning: ${unmatched} departments missing RR after join.`);
    }
  }).catch(err => {
    console.error(err);
    alert('Error building map: ' + err.message);
  });
})();
