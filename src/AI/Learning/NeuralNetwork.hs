module AI.Learning.NeuralNetwork where

import Numeric.LinearAlgebra
import Numeric.GSL.Minimization

import AI.Util.Matrix

-- |Representation for a single-hidden-layer neural network. If the network
--  has K input nodes, H hidden nodes and L output layers then the dimensions
--  of the matrices theta0 and theta1 are
--
--  * size theta0 = (K+1) x H
--  * size theta1 = (H+1) x L
--
--  the (+1)s account for the addition of bias nodes in the input layer and
--  the hidden layer. Therefore the total number of parameters is
--  H(K + L + 1) + L
data NeuralNetwork = NN (Matrix Double) (Matrix Double)

type NNShape = (Int,Int,Int)

-- |Make a prediction using a neural network.
nnPredict :: NeuralNetwork -> Matrix Double -> Matrix Double
nnPredict nn x = h where (_,_,h) = nnForwardProp nn x

-- |Perform forward propagation through a neural network, returning the matrices
--  created in the process.
nnForwardProp :: NeuralNetwork -> Matrix Double -> (Matrix Double, Matrix Double, Matrix Double)
nnForwardProp (NN theta0 theta1) x = (a0,a1,a2)
    where a0 = addOnes $ x
          a1 = addOnes $ sigmoidm (a0 <> theta0)
          a2 = sigmoidm (a1 <> theta1)

-- |Perform back and forward propagation through a neural network, returning the
--  final predictions (variable /a2/) and the gradient matrices (variables
--  /delta0/ and /delta1/) produced.
nnBackFwdProp :: NeuralNetwork -> Matrix Double -> Matrix Double -> (Matrix Double, Matrix Double, Matrix Double)
nnBackFwdProp nn@(NN theta0 theta1) y x = (a2, dropColumns 1 delta0, delta1)
    where
        (a0,a1,a2) = nnForwardProp nn x

        d2 = a2 - y
        d1 = (d2 <> trans theta1) * a1 * (1 - a1)

        delta0 = trans a0 <> d1
        delta1 = trans a1 <> d2

fromVector :: NNShape -> Vector Double -> NeuralNetwork
fromVector (k,h,l) vec = NN theta0 theta1
    where theta0 = reshape h $ takeVector ((k + 1) * h) vec
          theta1 = reshape l $ takeVector ((h + 1) * l) vec

toVector :: Matrix Double -> Matrix Double -> Vector Double
toVector theta0 theta1 = join [flatten theta0, flatten theta1]

-- |Back-propagation. Used to compute the cost function and gradient for the
--  neural network. The size of the matrices is as follows:
--
--  * theta0 is (K+1) x H
--  * theta1 is (H+1) x L
--  * a0 is T x K
--  * a1 is T x H
--  * a2 is T x L
--  * d2 is T x L
--  * d1 is T x (H+1)
--  * delta0 is (K+1) x (H+1)
--  * delta1 is (H+1) x L
nnCostGradient :: NNShape           -- (K,H,L)
               -> Matrix Double             -- targets (y)
               -> Matrix Double             -- design matrix (x)
               -> Double                    -- regularization parameter (lambda)
               -> Vector Double             -- neural network
               -> (Double, Vector Double)   -- (cost, gradient)
nnCostGradient shape y x lambda vec = (cost, grad)
    where
        m = fromIntegral (rows x)
        nn@(NN theta0 theta1) = fromVector shape vec
        (h, delta0, delta1)   = nnBackFwdProp nn y x

        cost  = (cost1 + cost2) / m
        cost1 = negate $ sumMatrix $ y * log h + (1-y) * log (1-h)
        cost2 = lambda/2 * (normMatrix theta0 + normMatrix theta1)

        grad  = (1/m) `scale` (grad1 + grad2)
        grad1 = toVector delta0 delta1
        grad2 = lambda `scale` toVector theta0 theta1

        normMatrix m = sumMatrix (dropRows 1 m ^ 2)

-- |Train a neural network from input vectors.
nnTrain :: NNShape -> Matrix Double -> Matrix Double -> Double -> NeuralNetwork
nnTrain shape y x lambda = fromVector shape vec
    where
        vec     = fst $ minimizeVD VectorBFGS2 prec niter sz1 tol cost grad (initialVec shape)
        prec    = 1e-9
        niter   = 1000
        sz1     = 0.1
        tol     = 0.1
        cost    = fst . nnCostGradient shape y x lambda
        grad    = snd . nnCostGradient shape y x lambda

initialVec :: NNShape -> Vector Double
initialVec (k,h,l) = constant 0 (h * (k + l) + h + l)

---------------
-- Utilities --
---------------

-- |Sigmoid function that acts on matrices.
sigmoidm :: Matrix Double -> Matrix Double
sigmoidm = mapMatrix $ \x -> 1 / (1 + exp (-x))

