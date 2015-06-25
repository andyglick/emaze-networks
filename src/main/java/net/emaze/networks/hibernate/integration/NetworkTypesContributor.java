package net.emaze.networks.hibernate.integration;

import net.emaze.networks.hibernate.Ipv4MaskType;
import net.emaze.networks.hibernate.Ipv4NetworkType;
import net.emaze.networks.hibernate.Ipv4Type;
import net.emaze.networks.hibernate.Ipv6MaskType;
import net.emaze.networks.hibernate.Ipv6NetworkType;
import net.emaze.networks.hibernate.Ipv6Type;
import net.emaze.networks.ipv4.Ipv4;
import net.emaze.networks.ipv4.Ipv4Mask;
import net.emaze.networks.ipv4.Ipv4Network;
import net.emaze.networks.ipv6.Ipv6;
import net.emaze.networks.ipv6.Ipv6Mask;
import net.emaze.networks.ipv6.Ipv6Network;
import org.hibernate.metamodel.spi.TypeContributions;
import org.hibernate.metamodel.spi.TypeContributor;
import org.hibernate.service.ServiceRegistry;

/**
 *
 * @author rferranti
 */
public class NetworkTypesContributor implements TypeContributor {

    @Override
    public void contribute(TypeContributions typeContributions, ServiceRegistry serviceRegistry) {
        typeContributions.contributeType(new Ipv4MaskType(), new String[]{Ipv4Mask.class.getName()});
        typeContributions.contributeType(new Ipv4NetworkType(), new String[]{Ipv4Network.class.getName()});
        typeContributions.contributeType(new Ipv4Type(), new String[]{Ipv4.class.getName()});
        typeContributions.contributeType(new Ipv6MaskType(), new String[]{Ipv6Mask.class.getName()});
        typeContributions.contributeType(new Ipv6NetworkType(), new String[]{Ipv6Network.class.getName()});
        typeContributions.contributeType(new Ipv6Type(), new String[]{Ipv6.class.getName()});
    }

}
