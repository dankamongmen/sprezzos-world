#ifndef RECT2D_HXX_
#define RECT2D_HXX_


#include <vigra/diff2d.hxx>
#include <vigra/tuple.hxx>


namespace vigra_ext
{

template <typename iterator, typename accessor>
inline vigra::triple<iterator, iterator, accessor>
apply(const vigra::Rect2D& rectangle, const vigra::triple<iterator, iterator, accessor>& image)
{
    return vigra::make_triple(image.first + rectangle.upperLeft(),
                              image.first + rectangle.lowerRight(),
                              image.third);
}

template <typename iterator, typename accessor>
inline std::pair<iterator, accessor>
apply(const vigra::Rect2D& rectangle, const std::pair<iterator, accessor>& image)
{
    return std::make_pair(image.first + rectangle.upperLeft(), image.second);
}

}

#endif // RECT2D_HXX_
