package net.emaze.networks.ipv6;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.function.BiFunction;
import net.emaze.dysfunctional.tuples.Pair;

public class Ipv6SubtractIpFromNetwork implements BiFunction<Ipv6Network, Ipv6, Set<Ipv6Network>> {

    @Override
    public Set<Ipv6Network> apply(Ipv6Network minuend, Ipv6 subtrahend) {
        return recursivelySubtract(minuend, subtrahend);
    }

    private Set<Ipv6Network> recursivelySubtract(Ipv6Network minuend, Ipv6 subtrahend) {
        if (!minuend.contains(subtrahend)) {
            return Collections.singleton(minuend);
        }
        if (minuend.netmask().isNarrowest()) {
            return Collections.emptySet();
        }
        final Set<Ipv6Network> reminder = new HashSet<>();
        final Pair<Ipv6Network, Ipv6Network> split = minuend.split();
        if (split.first().contains(subtrahend)) {
            reminder.add(split.second());
            reminder.addAll(recursivelySubtract(split.first(), subtrahend));
        } else {
            reminder.add(split.first());
            reminder.addAll(recursivelySubtract(split.second(), subtrahend));
        }
        return reminder;
    }

}
