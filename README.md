# Genart

https://genart.world

Genart is an interactive web app where one can create their own generative art. Its core mechanics are based on [L-Systems](https://en.wikipedia.org/wiki/L-system). Written in Elm and deployed via Netlify. This is currently a work in progress hobby project.

- [Examples](#examples)
- [Keyboard controls](#keyboard-controls)

### Examples

[Open in app](https://genart.world/editor?composition=%5B%22DLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLD%22%2C%22DLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLDDLDLD%22%5D&turnAngle=90.93889999999735&backgroundColor=%22%23000000%22&strokeColor=%22%231042b4%22&strokeWidth=0.018008170826353347&translateX=22.131194372558074&translateY=-39.18175179621922&scale=1.2000000000000002&curve=%22line%22)

<img width="500" alt="image" src="https://user-images.githubusercontent.com/4342234/133002563-1c12c0d1-c16c-494d-ba92-0c5a20b4820d.png">

[Open in app](https://genart.world/editor?composition=%5B%22DLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDL%22%2C%22DLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDLDL%22%5D&turnAngle=91.76536339382513&backgroundColor=%22%23000000%22&strokeColor=%22%23ff325f%22&strokeWidth=0.005140979886893275&translateX=-1.277270735894857&translateY=-1.9341998697531257&scale=1.0300000000000002&curve=%22curve%22)

<img width="500" alt="image" src="https://user-images.githubusercontent.com/4342234/133002612-11c3501c-6889-4b60-94c1-c6024f2edbb8.png">

[Open in app](https://genart.world/editor?composition=%5B%22DDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDD%22%2C%22DDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDDLDDD%22%2C%22DDDDDDDDDDDDDDD%22%5D&turnAngle=1111111110.9977298&backgroundColor=%22%238ddd55%22&strokeColor=%22%231084e7%22&strokeWidth=1&translateX=-11.00256947572367&translateY=2.0925638350619185&scale=1.23&curve=%22line%22)

<img width="500" alt="image" src="https://user-images.githubusercontent.com/4342234/133002768-58cc726f-d9cc-4831-bcd9-62d2e3aafbba.png">

[Open in app](https://genart.world/editor?composition=%5B%22D%22%2C%22DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD%22%2C%22DLDLDLDLDLDL%22%2C%22DD%22%2C%22DLD%22%5D&turnAngle=159.94664453611628&backgroundColor=%22%2300104d%22&strokeColor=%22%234dc7bc%22&strokeWidth=0.010313385377218032&translateX=1.6723226288274804&translateY=-0.9250000000000015&scale=0.7399999999999982&curve=%22curve%22)

<img width="500" alt="image" src="https://user-images.githubusercontent.com/4342234/133002669-496d3640-cade-43a6-b4a3-e63ede32c11f.png">

### Keyboard Controls

- up-arrow -- Add line segment to selected block
- left/right-arrow -- Turn left/right on selected block
- down-arrow -- Jump one line segment on selected block (go forward without drawing)
- backspace -- Delete last arrow command on selected block
- `i` -- Add a duplicate of selected block on top of all blocks
- `d` -- Delete the topmost block
- 1, 2, 3, ... 9, 0 -- Preset angles
- space bar -- Start/stop "video" (angle increasing or decreasing at each frame)
- `s` -- Slow motion on/off
- `,` -- Slow down video playback (same key as `<` on international keyboard)
- `.` -- Speed up video playback (same key as `>` on international keyboard)
- `r` -- Reverse direction of playback
- `o` -- Make some lines turn into smooth quadratic curves
- `l` -- Revert quadratic curves into straight lines
- `-` -- Zoom out
- `_` -- Zoom out (smaller steps)
- `=` -- Zoom in
- `+` -- Zoom in (smaller steps)
- `c` -- Reset zoom and pan
- `a` -- Focus on angle input (while there, up/down-arrow will change the angle at playback rate)
- `[` -- Make stroke width thinner
- `]` -- Make stroke width thicker

---

Made with <3 from Brazil.
