{-# LANGUAGE OverloadedStrings #-}
module Graphics.Text.Shader where

import           Graphics.Rendering.OpenGL hiding (Bitmap, Matrix)
import           Foreign
import qualified Data.ByteString as B


vertSrc :: B.ByteString
vertSrc = B.intercalate "\n"
    [ "attribute vec2 position;"
    , "attribute vec2 uv;"

    , "varying vec2 vTex; "

    , "uniform mat4 modelview;"
    , "uniform mat4 projection;"

    , "void main () {"
    , "    vTex = uv;"
    , "    gl_Position = projection * modelview * vec4(position, 0.0, 1.0);"
    , "}"
    ]


fragSrc :: B.ByteString
fragSrc = B.intercalate "\n"
    [ "varying vec2 vTex;"

    , "uniform sampler2D sampler;"
    , "uniform vec4 color;"

    , "void main() {"
    , "    vec4 tc = texture2D(sampler, vec2(vTex.s,vTex.t));"
    , "    gl_FragColor = vec4(1,0,0,1);//vec4(color.r,color.g,color.b,tc.r);"
    , "}"
    ]


vertDescriptor :: VertexArrayDescriptor [Float]
vertDescriptor = VertexArrayDescriptor 2 Float 0 nullPtr


uvDescriptor :: VertexArrayDescriptor [Float]
uvDescriptor = vertDescriptor
