import json
import math
from random import random, uniform

materials = {}
world = []

materials['ground'] = {
    'type': 'Diffuse',
    'texture': {
        'type': 'SolidColor',
        'color': (0.5, 0.5, 0.5)
    }
}
world.append({
    'type': 'Sphere',
    'center': (0, -1000, 0),
    'radius': 1000,
    'material': 'ground'
})

def dist(p0, p1): return math.sqrt((p1[0] - p0[0])**2 + (p1[1] - p0[1])**2 + (p1[2] - p0[2])**2)

i = 0
for a in range(-11, 11):
    for b in range(-11, 11):
        choose_mat = random()
        center = (a + 0.9*random(), 0.2, b + 0.9*random())

        if dist(center, (4, 0.2, 0)) > 0.9:
            i += 1
            if choose_mat < 0.8:
                # diffuse
                materials[f'mat{i}'] = {
                    'type': 'Diffuse',
                    'texture': {
                        'type': 'SolidColor',
                        'color': (random()*random(), random()*random(), random()*random())
                    }
                }
            elif choose_mat < 0.95:
                # metal
                materials[f'mat{i}'] = {
                    'type': 'Glossy',
                    'roughness': uniform(0, 0.5),
                    'texture': {
                        'type': 'SolidColor',
                        'color': (uniform(0.5, 1), uniform(0.5, 1), uniform(0.5, 1))
                    }
                }
            else:
                # glass
                materials[f'mat{i}'] = {
                    'type': 'Glass',
                    'ior': 1.5,
                    'texture': {
                        'type': 'SolidColor',
                        'color': (1, 1, 1)
                    }
                }
            world.append({
                'type': 'Sphere',
                'center': center,
                'radius': 0.2,
                'material': f'mat{i}'
            })

materials['glass'] = {
    'type': 'Glass',
    'ior': 1.5,
    'texture': {
        'type': 'SolidColor',
        'color': (1, 1, 1)
    }
}
world.append({
    'type': 'Sphere',
    'center': (0, 1, 0),
    'radius': 1,
    'material': 'glass'
})

materials['diffuse'] = {
    'type': 'Diffuse',
    'texture': {
        'type': 'SolidColor',
        'color': (0.4, 0.2, 0.1)
    }
}
world.append({
    'type': 'Sphere',
    'center': (-4, 1, 0),
    'radius': 1,
    'material': 'diffuse'
})

materials['metal'] = {
    'type': 'Glossy',
    'roughness': 0,
    'texture': {
        'type': 'SolidColor',
        'color': (0.7, 0.6, 0.5)
    }
}
world.append({
    'type': 'Sphere',
    'center': (4, 1, 0),
    'radius': 1,
    'material': 'metal'
})

camera = {
    'origin': (13, 2, 3),
    'target': (0, 0, 0),
    "vup": [0, 1, 0],
    "vfov": 20,
    "aspect_ratio": 1.78,
    "aperture": 0.1,
    "focus_distance": 10,
    "background": [0.70, 0.80, 1.00]
}

with open('random.json', 'w') as f:
    json.dump({
        'camera': camera,
        'materials': materials,
        'world': world
    }, f, indent=4)
