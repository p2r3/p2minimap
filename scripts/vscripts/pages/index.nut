::sendResponse(@"

<!DOCTYPE html>
  <html lang='en'>
  <head>
    <meta charset='UTF-8'>
    <meta name='viewport' content='width=device-width, initial-scale=1.0'>
    <style>
      body {
        background-color: black;
        color: black;
        margin: 15px;
      }
      body * {
        color: white;
        font-family: sans-serif;
      }
      button {
        border: 2px solid white;
        border-radius: 5px;
        background: black;
        padding: 10px;
        outline: none;
      }
      canvas {
        border: 2px solid white;
        border-radius: 15px;
        display: inline-block;
        margin: 0 15px;
      }
    </style>
    <title>Portal 2 Server</title>
  </head>
  <body>

    <h1>Hello from Portal 2!</h1>
    <br>
    <p>Map name: <b id='map-name'></b></p>
    <p>Player position: <b id='player-position'></b></p>

    <canvas id='ourCanvas' width='500' height='500'></canvas>
    <canvas id='theirCanvas' width='500' height='500'></canvas>

    <script>

      const canvas = document.getElementById('ourCanvas');
      const canvas2 = document.getElementById('theirCanvas');
      const ctx = canvas.getContext('2d');
      const ctx2 = canvas2.getContext('2d');
      const width = canvas.width;
      const height = canvas.height;
      ctx.lineWidth = 2;
      ctx2.lineWidth = 2;

      async function fetchGameJSON (content) {

        const headers = {};
        headers['P2-' + content] = '';

        try {
          const response = await fetch('/', {
            signal: AbortSignal.timeout(200),
            headers
          });
          return await response.json();
        } catch (e) {
          return null;
        }

      }

      const mapName = document.querySelector('#map-name');
      const playerPosition = document.querySelector('#player-position');

      fetchGameJSON('MapName').then(map => mapName.textContent = map);

      const entColors = {
        'prop_weighted_cube': '#5577ff',
        'prop_portal': '#ff0000',
        'prop_testchamber_door': '#ffffff',
      };

      setInterval(async function () {

        const points = await fetchGameJSON('TopDown');
        const entities = await fetchGameJSON('Entities');
        const slice = await fetchGameJSON('Slice');

        for (const point of points) point[0] = -point[0];

        const pos = { x: points[0][0], y: points[0][1] };
        const fvec = { x: points[1][0], y: points[1][1] };

        playerPosition.textContent = `${pos.x} ${pos.y}`;

        ctx.clearRect(0, 0, width, height);
        ctx2.clearRect(0, 0, width, height);

        ctx.beginPath();
        for (let i = 2; i < points.length; i ++) {

          const x = points[i][0] / 2048 * width + width / 2;
          const y = points[i][1] / 2048 * height + height / 2;

          if (i === 1) ctx.moveTo(x, y);
          else ctx.lineTo(x, y);

        }
        ctx.strokeStyle = 'white';
        ctx.fillStyle = '#111';
        ctx.closePath();
        ctx.fill();
        ctx.stroke();

        ctx.beginPath();
        ctx.moveTo(width / 2, height / 2);
        ctx.lineTo(fvec.x * 32 + width / 2, fvec.y * 32 + height / 2);
        ctx.strokeStyle = 'red';
        ctx.stroke();

        ctx.beginPath();
        ctx.arc(width / 2, height / 2, 5, 0, Math.PI * 2);
        ctx.fillStyle = 'red';
        ctx.fill();

        for (const entclass in entities) {
          for (const ent of entities[entclass]) {
            ctx.beginPath();
            ctx.arc((-ent[0] - pos.x) / 2048 * width + width / 2, (ent[1] - pos.y) / 2048 * height + height / 2, 5, 0, Math.PI * 2);
            ctx.fillStyle = entColors[entclass];
            ctx.fill();
          }
        }

        ctx2.beginPath();
        ctx2.moveTo(slice[1][0] * width, -slice[1][1] * height + height / 2);

        for (let i = 2; i < slice.length; i ++) {
          ctx2.lineTo(slice[i][0] * width, -slice[i][1] * height + height / 2);
        }
        ctx2.strokeStyle = 'white';
        ctx2.stroke();

        const pitch = slice[0] * Math.PI / 180;

        ctx2.beginPath();
        ctx2.moveTo(0, height / 2);
        ctx2.lineTo(Math.cos(pitch) * width, height / 2 + Math.sin(pitch) * width);
        ctx2.strokeStyle = 'red';
        ctx2.stroke();

      }, 200);

      const inputFunction = function (event, plus) {

        let input = null;

        switch (event.key.toLowerCase()) {
          case 'w': { input = 'forward'; break }
          case 'a': { input = 'moveleft'; break }
          case 's': { input = 'back'; break }
          case 'd': { input = 'moveright'; break }
          case ' ': { input = 'jump'; break }
          default: return;
        }

        fetch('/', {
          method: 'POST',
          headers: { 'Command': `;${plus ? '+' : '-'}${input}` }
        });

      };

      window.addEventListener('keydown', function (event) {
        inputFunction(event, true);
      });
      window.addEventListener('keyup', function (event) {
        inputFunction(event, false);
      });

    </script>

  </body>
</html>

");