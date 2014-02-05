package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Ipv6Mask;
import org.junit.Test;

public class Ipv6MaskFromStringTest1 {

    @Test
    public void deserializingIPv6MaskYieldsExpected() throws IOException {
        final String serialized = "{'netmask':115}".replace("'", "\"");
        final Ipv6Mask expected = Ipv6Mask.net(115);
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithIpv6Mask got = mapper.readValue(serialized, BeanWithIpv6Mask.class);
        Assert.assertEquals(expected, got.getNetmask());
    }
}
