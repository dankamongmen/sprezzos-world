/*
 * Copyright (C) 2012 Dr. Christoph L. Spiel
 *
 * This file is part of Enblend.
 *
 * Enblend is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Enblend is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Enblend; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */


#ifndef FILLPOLYGON_HXX
#define FILLPOLYGON_HXX


#include <algorithm>
#include <cassert>
#include <list>
#include <stdexcept>
#include <vector>

#include <vigra/diff2d.hxx>

#ifndef HAVE_LRINT
__inline long int lrint (double x){
    return static_cast<long int>(x + (x < 0.0 ? -0.5 : 0.5));
}
#endif


#define END_OF_SEGMENT_MARKER vigra::Point2D(INT_MIN, INT_MIN)


namespace vigra_ext
{
    namespace detail
    {
        template <class PolygonVertexIterator>
        inline static vigra::Rect2D
        get_polygon_extent(PolygonVertexIterator vertex_begin, PolygonVertexIterator vertex_end)
        {
            if (vertex_begin == vertex_end)
            {
                throw std::invalid_argument("empty polygon");
            }

            int left = vertex_begin->px();
            int top = vertex_begin->py();
            int right = vertex_begin->px();
            int bottom = vertex_begin->py();

            for (PolygonVertexIterator v = vertex_begin; v != vertex_end; ++v)
            {
                if (*v != END_OF_SEGMENT_MARKER)
                {
                    left = std::min(left, v->px());
                    top = std::min(top, v->py());
                    right = std::max(right, v->px());
                    bottom = std::max(bottom, v->py());
                }
            }

            return vigra::Rect2D(left, top, right, bottom);
        }


        typedef enum {HORIZONTAL_LEFT, CROSSING, TOUCHING, HORIZONTAL_RIGHT} intersection_t;


        inline static intersection_t
        intersection_of_bool(bool is_touching)
        {
            return is_touching ? TOUCHING : CROSSING;
        }


        inline static bool
        is_touching_point(const vigra::Point2D& u, const vigra::Point2D& v, int y)
        {
            return (u.py() >= y && v.py() >= y) || (u.py() < y && v.py() < y);
        }


        template <class PolygonVertexIterator, class BackInsertIterator>
        inline void
        search_intersections(const PolygonVertexIterator& vertex_begin,
                             const PolygonVertexIterator& vertex_end,
                             int y, // y-coordinate of scanline
                             BackInsertIterator intersections_end)
        {
            if (vertex_begin == vertex_end)
            {
                return;
            }

            PolygonVertexIterator u(vertex_begin);
            PolygonVertexIterator v(vertex_begin);

            ++v;
            while (v != vertex_end)
            {
                if (*u != END_OF_SEGMENT_MARKER && *v != END_OF_SEGMENT_MARKER)
                {
                    const int delta_y = v->py() - u->py();
                    if (delta_y != 0)
                    {
                        const double m = static_cast<double>(y - u->py()) / static_cast<double>(delta_y);
                        if (0.0 <= m && m <= 1.0)
                        {
                            const int x = lrint(static_cast<double>(u->px()) +
                                                m * static_cast<double>(v->px() - u->px()));
                            *intersections_end++ =
                                std::make_pair(x, intersection_of_bool(is_touching_point(*u, *v, y)));
                        }
                    }
                    else if (y == u->py()) // horizontal segment in _current_ scanline
                    {
                        *intersections_end++ = std::make_pair(std::min(u->px(), v->px()), HORIZONTAL_LEFT);
                        *intersections_end++ = std::make_pair(std::max(u->px(), v->px()), HORIZONTAL_RIGHT);
                    }
                }

                ++u;
                ++v;
            }
        }


        template<class ActiveList, class BackInsertIterator>
        inline void
        search_intersections_active(ActiveList& active_segments,
                                    int y, // y-coordinate of scanline
                                    BackInsertIterator intersections_end)
        {
            typedef typename ActiveList::iterator active_list_iterator;

            active_list_iterator a(active_segments.begin());

            while (a != active_segments.end())
            {
                if (a->first != END_OF_SEGMENT_MARKER && a->second != END_OF_SEGMENT_MARKER)
                {
                    const int delta_y = a->second.py() - a->first.py();
                    if (delta_y != 0)
                    {
                        const double m = static_cast<double>(y - a->first.py()) / static_cast<double>(delta_y);

                        if (0.0 <= m && m <= 1.0)
                        {
                            const int x = lrint(static_cast<double>(a->first.px()) +
                                                m * static_cast<double>(a->second.px() - a->first.px()));
                            *intersections_end++ =
                                std::make_pair(x, intersection_of_bool(is_touching_point(a->first, a->second, y)));

                            if (m == 1.0)
                            {
                                active_segments.erase(a++); // see: Meyers, Effective STL, Item 9
                                continue;
                            }
                        }
                    }
                    else if (y == a->first.py()) // horizontal segment in _current_ scanline
                    {
                        *intersections_end++ = std::make_pair(std::min(a->first.px(), a->second.px()), HORIZONTAL_LEFT);
                        *intersections_end++ = std::make_pair(std::max(a->first.px(), a->second.px()), HORIZONTAL_RIGHT);
                    }
                }

                ++a;
            }
        }


        template <class RowIterator, class Accessor>
        inline static void
        fill_row(const RowIterator& row, int x_first, int x_last,
                 Accessor accessor, const typename Accessor::value_type& value)
        {
            for (int x = x_first; x <= x_last; ++x)
            {
                accessor.set(value, row, x);
            }
        }


        template <typename T>
        inline static T
        limit(T x, T lower_limit, T upper_limit)
        {
            return std::min(std::max(x, lower_limit), upper_limit);
        }


        struct malformed_polygon : public std::runtime_error
        {
            malformed_polygon(const std::string& message) : std::runtime_error(message) {}
        };


        template <class IntersectionList, class RowIterator, class Accessor>
        inline static void
        fill_row_segments(const IntersectionList& intersections,
                          const RowIterator& row, int row_width, Accessor accessor,
                          const typename Accessor::value_type& value)
        {
            typedef typename IntersectionList::size_type size_type;

            const size_type n = intersections.size();
            const int row_max = row_width - 1;

            if (n >= 2U && n % 2U == 0U)
            {
                for (size_type i = 0U; i != n; i += 2U)
                {
                    const int x0 = limit(intersections[i], 0, row_max);
                    const int x1 = limit(intersections[i + 1], 0, row_max);

                    detail::fill_row(row, x0, x1, accessor, value);
                }
            }
            else
            {
                throw malformed_polygon("vigra_ext::fill_row_segments: open polygon");
            }
        }


        template <class Iterator, class BackInsertIterator>
        void
        group_to_pairs(Iterator begin, Iterator end, BackInsertIterator result)
        {
            typedef typename Iterator::value_type value_type;
            typedef std::vector<typename value_type::first_type> array;
            typedef typename array::const_iterator array_const_iterator;

            array horizontal_begins;
            array horizontal_ends;

            for (Iterator i = begin; i != end; ++i)
            {
                switch (i->second)
                {
                case TOUCHING:
                    *result++ = i->first;
                    // FALLTHROUGH

                case CROSSING:
                    *result++ = i->first;
                    break;

                case HORIZONTAL_LEFT:
                    horizontal_begins.push_back(i->first);
                    break;

                case HORIZONTAL_RIGHT:
                    horizontal_ends.push_back(i->first);
                    break;

                default:
                    assert(false);
                }
            }

            assert(horizontal_begins.size() == horizontal_ends.size());

            array_const_iterator x0(horizontal_begins.begin());
            array_const_iterator x1(horizontal_ends.begin());

            while (x0 != horizontal_begins.end())
            {
                *result++ = *x0++;
                *result++ = *x1++;
            }
        }


        template <class SegmentType>
        struct LessThanSegment :
            public std::binary_function<SegmentType, SegmentType, bool>
        {
            bool operator()(const SegmentType& s, const SegmentType& t) const
            {
                const int s_min_y = std::min(s.first.py(), s.second.py());
                const int t_min_y = std::min(t.first.py(), t.second.py());

                if (s_min_y < t_min_y)
                {
                    return true;
                }
                else if (s_min_y == t_min_y)
                {
                    const int s_min_x = std::min(s.first.px(), s.second.px());
                    const int t_min_x = std::min(t.first.px(), t.second.px());

                    return s_min_x < t_min_x;
                }
                else
                {
                    return false;
                }
            }
        };
    } // end namespace detail


    template <class ImageIterator, class ImageAccessor, class ValueType, class PolygonVertexIterator>
    void
    fill_polygon(const ImageIterator& upper_left, const ImageIterator& lower_right, const ImageAccessor& accessor,
                 const PolygonVertexIterator& vertex_begin, const PolygonVertexIterator& vertex_end,
                 const ValueType& fill_value)
    {
        typedef std::pair<int, detail::intersection_t> intersection_data;
        typedef std::vector<intersection_data> intersection_list;
        typedef typename ImageIterator::row_iterator row_iterator;

        if (vertex_begin == vertex_end)
        {
            return;
        }

        const vigra::Size2D image_size(lower_right - upper_left);
        const vigra::Rect2D extent(detail::get_polygon_extent(vertex_begin, vertex_end));

#ifdef DEBUG_POLYGON_FILL
        std::cout <<
            "+ fill_polygon: image's size = " << image_size << "\n" <<
            "+ fill_polygon: polygon's extent = " << extent;
        unsigned i = 0U;
        for (PolygonVertexIterator v = vertex_begin; v != vertex_end; ++v)
        {
            if (i % 5U == 0U)
            {
                std::cout << "\n+ fill_polygon: vertices    ";
            }
            else
            {
                std::cout << "    ";
            }

            if (*v == END_OF_SEGMENT_MARKER)
            {
                std::cout << "<EOS>";
            }
            else
            {
                std::cout << *v;
            }

            ++i;
        }
        std::cout << "\n";
#endif

#ifdef OPENMP
#pragma omp for
#endif
        for (int y = std::max(0, extent.top()); y < std::min(image_size.height(), extent.bottom()); ++y)
        {
            intersection_list intersections;
            detail::search_intersections(vertex_begin, vertex_end, y, std::back_inserter(intersections));

            if (!intersections.empty()) // OPTIMIZATION: skip empty scanlines
            {
                std::sort(intersections.begin(), intersections.end());

                std::vector<int> paired_intersections;
                paired_intersections.reserve(intersections.size());
                detail::group_to_pairs(intersections.begin(), intersections.end(), std::back_inserter(paired_intersections));

                const row_iterator row((upper_left + vigra::Diff2D(0, y)).rowIterator());
                detail::fill_row_segments(paired_intersections,
                                          row, image_size.width(), accessor,
                                          fill_value);
            }
        }
    }


    template <class ImageIterator, class ImageAccessor, class ValueType, class PolygonVertexIterator>
    void
    fill_polygon_active(const ImageIterator& upper_left, const ImageIterator& lower_right, const ImageAccessor& accessor,
                        const PolygonVertexIterator& vertex_begin, const PolygonVertexIterator& vertex_end,
                        const ValueType& fill_value)
    {
        typedef std::pair<int, detail::intersection_t> intersection_data;
        typedef std::vector<intersection_data> intersection_list;
        typedef typename ImageIterator::row_iterator row_iterator;

        if (vertex_begin == vertex_end)
        {
            return;
        }

        const vigra::Size2D image_size(lower_right - upper_left);
        const vigra::Rect2D extent(detail::get_polygon_extent(vertex_begin, vertex_end));

        typedef std::pair<vigra::Point2D, vigra::Point2D> segment;
        typedef std::vector<segment> segments;
        typedef typename segments::const_iterator segments_const_iterator;
        typedef std::list<segment> segment_list;

        // Create the line segments that make up the polygon.  We sort the
        // y-coordinates ascendingly.
        segments polygon_segments;
        PolygonVertexIterator u(vertex_begin);
        PolygonVertexIterator v(vertex_begin);

        ++v;
        while (v != vertex_end)
        {
            polygon_segments.push_back(u->py() < v->py() ? std::make_pair(*u, *v) : std::make_pair(*v, *u));
            ++u;
            ++v;
        }

        // Sort the line segments according to their y-coordinates.
        std::sort(polygon_segments.begin(), polygon_segments.end(), detail::LessThanSegment<segment>());

        segment_list active_segments;
        segments_const_iterator s(polygon_segments.begin());

#ifdef OPENMP
#pragma omp parallel for private (active_segments) firstprivate (s)
#endif
        for (int y = std::max(0, extent.top()); y < std::min(image_size.height(), extent.bottom()); ++y)
        {
            // Fill active-segments range.
            while (s != polygon_segments.end() && s->first.py() <= y)
            {
                // NOTE: In the parallel version all threads start at
                // polygon_segments.begin(), but we need to record
                // only segments that reach beyond our scan-line.
                if (s->second.py() >= y)
                {
                    active_segments.push_back(*s);
                }
                ++s;
            }

            intersection_list intersections;
            detail::search_intersections_active(active_segments, y, std::back_inserter(intersections));

            if (!intersections.empty()) // OPTIMIZATION: skip empty scanlines
            {
                std::sort(intersections.begin(), intersections.end());

                std::vector<int> paired_intersections;
                paired_intersections.reserve(intersections.size());
                detail::group_to_pairs(intersections.begin(), intersections.end(), std::back_inserter(paired_intersections));

                const row_iterator row((upper_left + vigra::Diff2D(0, y)).rowIterator());
                detail::fill_row_segments(paired_intersections,
                                          row, image_size.width(), accessor,
                                          fill_value);
            }
        }
    }
} // end namespace vigra_ext


#endif // FILLPOLYGON_HXX
