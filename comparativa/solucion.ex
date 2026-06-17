defmodule Transicion do
  def transicion(:en_rojo, :amarillo) do
    {:en_rojo, "cambiar-a-amarillo"}
  end

  def transicion(:en_amarillo, :verde) do
    {:en_amarillo, "cambiar-a-verde"}
  end

  def transicion(:en_verde, :rojo) do
    {:en_verde, "cambiar-a-rojo"}
  end

  def transicion(color_actual, _cambiar_a) do
    {color_actual, "accion-por-defecto"}
  end
end
