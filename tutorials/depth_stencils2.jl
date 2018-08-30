using ModernGL, GeometryTypes, GLAbstraction, GLWindow, Images, FileIO
import GLAbstraction: StandardPostrender
# Load our textures. See "downloads.jl" to get the images.
kitten = load(GLAbstraction.dir("tutorials", "images", "kitten.png"))
puppy  = load(GLAbstraction.dir("tutorials", "images", "puppy.png"))

windowhints = [(GLFW.DEPTH_BITS, 32), (GLFW.STENCIL_BITS, 8)]
window = create_glcontext("Depth and stencils 2",
                          resolution=(600,600),
                          windowhints=windowhints)

vao = glGenVertexArrays()
glBindVertexArray(vao)

# The cube. This could be more efficiently represented using indexes,
# but the tutorial doesn't do it that way.
vertex_positions = Vec3f0[
    # The cube
    (-0.5f0, -0.5f0, -0.5f0),
    ( 0.5f0, -0.5f0, -0.5f0),
    ( 0.5f0,  0.5f0, -0.5f0),
    (-0.5f0,  0.5f0, -0.5f0),

    (-0.5f0, -0.5f0,  0.5f0),
    ( 0.5f0, -0.5f0,  0.5f0),
    ( 0.5f0,  0.5f0,  0.5f0),
    (-0.5f0,  0.5f0,  0.5f0),

    (-0.5f0,  0.5f0,  0.5f0),
    (-0.5f0,  0.5f0, -0.5f0),
    (-0.5f0, -0.5f0, -0.5f0),
    (-0.5f0, -0.5f0,  0.5f0),

    ( 0.5f0,  0.5f0,  0.5f0),
    ( 0.5f0,  0.5f0, -0.5f0),
    ( 0.5f0, -0.5f0, -0.5f0),
    ( 0.5f0, -0.5f0,  0.5f0),

    (-0.5f0, -0.5f0, -0.5f0),
    ( 0.5f0, -0.5f0, -0.5f0),
    ( 0.5f0, -0.5f0,  0.5f0),
    (-0.5f0, -0.5f0,  0.5f0),

    (-0.5f0,  0.5f0, -0.5f0),
    ( 0.5f0,  0.5f0, -0.5f0),
    ( 0.5f0,  0.5f0,  0.5f0),
    (-0.5f0,  0.5f0,  0.5f0)]

floor_positions = Vec3f0[
    # The floor
    (-1.0f0, -1.0f0, -0.5f0),
    ( 1.0f0, -1.0f0, -0.5f0),
    ( 1.0f0,  1.0f0, -0.5f0),
    (-1.0f0,  1.0f0, -0.5f0)
]

vertex_texcoords = Vec2f0[
                          # The cube
                          (0.0f0, 0.0f0),
                          (1.0f0, 0.0f0),
                          (1.0f0, 1.0f0),
                          (0.0f0, 1.0f0),

                          (0.0f0, 0.0f0),
                          (1.0f0, 0.0f0),
                          (1.0f0, 1.0f0),
                          (0.0f0, 1.0f0),

                          (1.0f0, 0.0f0),
                          (1.0f0, 1.0f0),
                          (0.0f0, 1.0f0),
                          (0.0f0, 0.0f0),

                          (1.0f0, 0.0f0),
                          (1.0f0, 1.0f0),
                          (0.0f0, 1.0f0),
                          (0.0f0, 0.0f0),

                          (0.0f0, 1.0f0),
                          (1.0f0, 1.0f0),
                          (1.0f0, 0.0f0),
                          (0.0f0, 0.0f0),

                          (0.0f0, 1.0f0),
                          (1.0f0, 1.0f0),
                          (1.0f0, 0.0f0),
                          (0.0f0, 0.0f0)]

floor_texcoords = Vec2f0[
                          # The floor
                          (0.0f0, 0.0f0),
                          (1.0f0, 0.0f0),
                          (1.0f0, 1.0f0),
                          (0.0f0, 1.0f0)]

vertex_colors = fill(Vec3f0(1,1,1), 24)
floor_colors = fill(Vec3f0(0,0,0),4)

elements_cube = Face{3,UInt32}[( 0, 1, 2),
	                       ( 2, 3, 0),

		               ( 4, 5, 6),
			       ( 6, 7, 4),

			       ( 8, 9,10),
			       (10,11, 8),

			       (12,13,14),
			       (14,15,12),

			       (16,17,18),
			       (18,19,16),

			       (20,21,22),
			       (22,23,20)]

elements_floor = Face{3,UInt32}[(0,1,2),
				(2,3,0)]


vertex_shader = vert"""
#version 150

in vec3 position;
in vec3 color;
in vec2 texcoord;

out vec3 Color;
out vec2 Texcoord;

uniform vec3 overrideColor;
uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;

void main()
{
    Color = overrideColor * color;
    Texcoord = texcoord;
    gl_Position = proj * view * model * vec4(position, 1.0);
}
"""

fragment_shader = frag"""
#version 150

in vec3 Color;
in vec2 Texcoord;

out vec4 outColor;

uniform sampler2D texKitten;
uniform sampler2D texPuppy;

void main()
{
    vec4 colKitten = texture(texKitten, Texcoord);
    vec4 colPuppy = texture(texPuppy, Texcoord);
    outColor = vec4(Color, 1.0) * mix(colKitten, colPuppy, 0.5);
}
"""

model1 = eye(Mat{4,4,Float32})
model2 = translationmatrix_z(-1f0) * scalematrix(Vec3f0(1,1,-1))
view = lookat(Vec3f0((2.5, 2.5, 2)), Vec3f0((0, 0, 0)), Vec3f0((0, 0, 1)))
proj = perspectiveprojection(Float32, 45, 600/600, 1, 10)


## Now render the distinct objects. Rather than always using std_renderobject,
## here we control the settings manually.
# The cube
bufferdict_cube = Dict(:position=>GLBuffer(vertex_positions),
                       :texcoord=>GLBuffer(vertex_texcoords),
                       :color=>GLBuffer(vertex_colors),
                       :texKitten=>Texture(kitten'),
                       :texPuppy=>Texture(puppy'),
                       :overrideColor=>Vec3f0((1,1,1)),
                       :model=>model1,
                       :view=>view,
                       :proj=>proj,
		       :indexes=>indexbuffer(elements_cube))

pre = () -> begin
    glEnable(GL_DEPTH_TEST)
    glDepthMask(GL_TRUE)
    glDepthFunc(GL_LEQUAL)
    glDisable(GL_CULL_FACE)
    enabletransparency()
    glDisable(GL_STENCIL_TEST)
end

ro_cube = RenderObject(
    bufferdict_cube,
    LazyShader(vertex_shader, fragment_shader),
     pre, nothing, AABB(Vec3f0(0), Vec3f0(1)), nothing
)
ro_cube.postrenderfunction = StandardPostrender(ro_cube.vertexarray, GL_TRIANGLES)

# The floor. This is drawn without writing to the depth buffer, but we
# write stencil values.
bufferdict_floor = Dict(:position=>GLBuffer(floor_positions),
                        :texcoord=>GLBuffer(floor_texcoords),
                        :color=>GLBuffer(floor_colors),
                        :texKitten=>Texture(kitten'), # with different shaders, wouldn't need these here
                        :texPuppy=>Texture(puppy'),
                        :overrideColor=>Vec3f0((1,1,1)),
                        :model=>model1,
                        :view=>view,
                        :proj=>proj,
			:indexes=>indexbuffer(elements_floor))

function prerender()
    glDepthMask(GL_FALSE)                  # don't write to depth buffer
    glEnable(GL_STENCIL_TEST)              # use stencils
    glStencilMask(0xff)                    # do write to stencil buffer
    glStencilFunc(GL_ALWAYS, 1, 0xff)      # all pass
    glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE)  # replace stencil value
    glClear(GL_STENCIL_BUFFER_BIT)         # start with empty buffer
end
ro_floor = RenderObject(
    bufferdict_floor,
    LazyShader(vertex_shader, fragment_shader),
    prerender, nothing, AABB(Vec3f0(0), Vec3f0(1)), nothing
)
ro_floor.postrenderfunction = StandardPostrender(ro_floor.vertexarray, GL_TRIANGLES)

# The cube reflection
bufferdict_refl = Dict(:position=>GLBuffer(vertex_positions),
                       :texcoord=>GLBuffer(vertex_texcoords),
                       :color=>GLBuffer(vertex_colors),
                       :texKitten=>Texture(kitten'),
                       :texPuppy=>Texture(puppy'),
                       :overrideColor=>Vec3f0((0.3,0.3,0.3)),
                       :model=>model2,
                       :view=>view,
                       :proj=>proj,
		       :indexes=>indexbuffer(elements_cube))

pre = () -> begin
    glEnable(GL_DEPTH_TEST)
    glDepthMask(GL_TRUE)
    glDepthFunc(GL_LEQUAL)
    glDisable(GL_CULL_FACE)
    enabletransparency()
    glStencilFunc(GL_EQUAL, 1, 0xff)
    glStencilMask(0x00)
end

ro_refl = RenderObject(
   bufferdict_refl,
   LazyShader(vertex_shader, fragment_shader),
   pre, nothing, AABB(Vec3f0(0), Vec3f0(1)), nothing
)
ro_refl.postrenderfunction = StandardPostrender(ro_refl.vertexarray, GL_TRIANGLES)


glClearColor(1,1,1,1) # make the background white, so we can see the floor
glClearStencil(0)     # clear the stencil buffer with 0

while !GLFW.WindowShouldClose(window)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    GLAbstraction.render(ro_cube)
    GLAbstraction.render(ro_floor)
    GLAbstraction.render(ro_refl)

    GLFW.SwapBuffers(window)
    GLFW.PollEvents()
    if GLFW.GetKey(window, GLFW.KEY_ESCAPE) == GLFW.PRESS
        GLFW.SetWindowShouldClose(window, true)
    end
end
GLFW.DestroyWindow(window)  # needed if you're running this from the REPL
